use proc_macro2::TokenStream;

use crate::type_spec::TypeSpec;
use crate::types::Type;
use crate::types::Meta;
use crate::types::Endian;
use crate::types::Encoding;

pub const PRIMITIVES: &[&str] = &[
    "u8",
    "u16",
    "u32",
    "u64",
    "u128",
    "i8",
    "i16",
    "i32",
    "i64",
    "i128",
    "f32",
    "f64",
];

#[derive(Debug)]
pub enum Repeat {
    NoRepeat,
    Eos,
    Expr(TokenStream),
    Until(TokenStream),
}

impl Repeat {
    pub fn is_repeat(&self) -> bool {
        match self {
            Repeat::NoRepeat => false,
            _ => true,
        }
    }
}

impl Default for Repeat {
    fn default() -> Self {
        Repeat::NoRepeat
    }
}

#[derive(Default, Debug)]
pub struct Attribute {
    pub id: String,
    pub typ: String,
    pub enum_: Option<String>,
    pub repeat: Repeat,
    pub cond: Option<TokenStream>,
    pub contents: Vec<u8>,
    pub size: Option<SizeProperties>,
    pub encoding: Option<Encoding>
}

#[derive(Debug)]
pub enum Length {
    Eos,
    Terminator(u8),
    Size(TokenStream),
}

#[derive(Debug)]
pub struct SizeProperties {
    pub length: Length,
    pub consume: bool,
    pub include: bool,
    pub eos_error: bool,
}

pub struct Instance {
    pub attr: Attribute,
    pos: usize,
    pub value: Option<TokenStream>,
}

impl Attribute {
    pub fn new<T: Into<String>, U: Into<String>>(
        id: T,
        typ: U,
        repeat: Repeat,
        cond: Option<TokenStream>,
        contents: Vec<u8>,
        encoding: Option<Encoding>,
        enum_: Option<String>,
        size: Option<SizeProperties>) -> Self
    {
        Self {
            id: id.into(),
            typ: typ.into(),
            repeat,
            cond,
            encoding,
            contents,
            enum_,
            size,
        }
    }

    pub fn name(&self) -> TokenStream {
        syn::parse_str(&self.id.clone()).unwrap()
    }

    pub fn resolve_scalar_type(&self, structure: &TypeSpec) -> Type {
        let path = self.typ.split('.').collect();
        structure.resolve_type(path)
    }

    pub fn absolute_path_of_compound_type(&self, structure: &TypeSpec) -> TokenStream {
        let scalar = if self.contents.is_empty() {
            let scalar = self.resolve_scalar_type(&structure);
            scalar.absolute_final_path()
        } else {
            quote!{ () }
        };

        let scalar = if self.size.is_some() && self.typ != "str" && !self.repeat.is_repeat() {
            quote!( std::vec::Vec<#scalar> )
        } else {
            scalar
        };

        let compound = match self.repeat {
            Repeat::NoRepeat => scalar,
            _ => quote!( std::vec::Vec<#scalar> ),
        };

        if self.cond.is_some() {
            quote!( std::option::Option<#compound> )
        } else {
            compound
        }
    }

    pub fn read_call(&self,
        typ: &Type,
        parent_precursors: &[TokenStream],
        root_precursor: TokenStream,
        meta: &Meta) -> TokenStream
    {
        let primitive_with_endian = |p: &&str| {
            self.typ.starts_with(p) &&
            self.typ.len() == p.len() + 2 &&
            (self.typ.ends_with("le") || self.typ.ends_with("be"))
        };

        let read_size = self.size.as_ref().map(|size| {
            let to_vec = quote!( |v: &[u8]| -> std::result::Result<Vec<u8>, ()> { Ok(v.to_vec()) } );
            match &size.length {
                Length::Size(len) => quote!( map_res!(take!(#len), #to_vec) ),
                Length::Terminator(chr) => quote!( map_res!(take_till!(|ch| ch == #chr), #to_vec) ),
                Length::Eos => quote!( map_res!(nom::rest, #to_vec) ),
            }
        });

        let read_scalar = match typ {
            Type::Primitive(_) if self.typ == "u8" && read_size.is_some() => {
                read_size.unwrap()
            }
            Type::Primitive(_) if PRIMITIVES.iter().any(primitive_with_endian) => {
                // primitive with endian (e.g. u8be)
                let (typ, endian) = &self.typ.split_at(self.typ.len() - 2);

                syn::parse_str(&format!("{}_{}", endian, typ)[..]).unwrap()
            }
            Type::Primitive(_) if PRIMITIVES.contains(&&self.typ[..]) => {
                // primitive without endian (e.g. u8)
                let big = syn::parse_str(&format!("be_{}", self.typ)[..]).unwrap();
                let little = syn::parse_str(&format!("le_{}", self.typ)[..]).unwrap();

                match &meta.endian {
                    Endian::Big => big,
                    Endian::Little => little,
                    Endian::Runtime(cond) => quote!(
                        match #cond {
                            Endian::Big => #big,
                            Endian::Little => #little,
                        }
                    )
                }
            },
            Type::Primitive(_) if self.typ == "str" => {
                // a 'str' or 'strz'
                let read_size = read_size.unwrap();

                let enc = match self.encoding.as_ref().unwrap_or(&meta.encoding) {
                    Encoding::Runtime(enc) => enc.clone(),
                    Encoding::Fixed(enc) => {
                        let enc = format!("{:?}", enc);
                        let enc: TokenStream = syn::parse_str(&enc[..]).unwrap();
                        quote!(#enc) // convert to str token
                    }
                };

                quote!( map_res!(#read_size, |v: Vec<u8>| decode_string(&v, &#enc)) )
            },
            Type::Custom(typ) => {
                // user-defined struct (e.g. super.header)
                let typ = typ.as_ref().borrow().absolute_final_path();
                let read_fn = TypeSpec::build_function_name("read", &parent_precursors, &Some(root_precursor));

                quote!(
                    apply!(#typ :: #read_fn, &_new_parents, _new_root, _meta, _ctx)
                )
            },
            _ => unimplemented!("Attribute::read_call(): type '{}' unknown", self.typ),
        };

        let read_scalar = if let Some(ref e) = self.enum_ {
            unimplemented!()
            //let e = typ.resolve_enum(e);
            //e.match_from_primitive(read_scalar)
        } else {
            read_scalar
        };

        let read_scalar = if self.contents.is_empty() {
            read_scalar
        } else {
            let contents: TokenStream;
            contents = syn::parse_str(&format!("&{:?}", self.contents)[..]).unwrap();
            quote!( do_parse!(tag!(#contents) >> (())) )
        };

        let read_compound = match &self.repeat {
            Repeat::NoRepeat => read_scalar,
            Repeat::Eos => quote!( many0!(complete!(#read_scalar)) ),
            //Repeat::Until(cond) => quote!( repeat_while!(#cond, #read_scalar) ), // many_till?
            Repeat::Until(_) => unimplemented!("Repeat::Until not implemented, yet"),
            Repeat::Expr(expr) => quote!( count!(#read_scalar, #expr) ),
        };

        let read_compound = if let Some(SizeProperties { length: Length::Size(len), .. }) = &self.size {
            match typ {
                Type::Custom(_) => {
                    quote! (
                        flat_map!(take!(#len), #read_compound)// FIXME: `len` expands to `self.len`, which cause borrow of self in nom 5.x
                    )
                }
                _ => read_compound
            }
        } else {
            read_compound
        };

        let read = if let Some(cond) = &self.cond {
            quote!( cond!(#cond, #read_compound) )
        } else {
            read_compound
        };

        quote!{
            do_parse!(_input, __v: #read >> (__v))
        }
    }

    pub fn write_call(&self,
        typ: &Type,
        parents: &[TokenStream],
        root: TokenStream,
        meta: &Meta) -> TokenStream
    {
        let primitive_with_endian = |p: &&str| {
            self.typ.starts_with(p) &&
            self.typ.len() == p.len() + 2 &&
            (self.typ.ends_with("le") || self.typ.ends_with("be"))
        };

        let write_scalar = match typ {
            Type::Primitive(_) if self.typ == "u8" && self.size.is_some() => {
                quote!(
                    _io.write_all
                )
            },
            Type::Primitive(_) if PRIMITIVES.iter().any(primitive_with_endian) => {
                // primitive with endian (e.g. u8be)
                let (styp, endian) = &self.typ.split_at(self.typ.len() - 2);

                let endian = match *endian {
                    "le" => quote!(byteorder::LittleEndian),
                    "be" => quote!(byteorder::BigEndian),
                    _ => panic!("Endian '{}' in '{}' unkown!", endian, self.typ),
                };

                let func: TokenStream = syn::parse_str(&format!("write_{}", styp)[..]).unwrap();

                let typ: TokenStream = syn::parse_str(&styp).unwrap();
                if styp == &"u8" {
                    quote!( (#[inline] |attr: &#typ| _io.#func(*attr)) )
                } else {
                    quote!( (#[inline] |attr: &#typ| _io.#func::<#endian>(*attr)) )
                }
            }
            Type::Primitive(_) if PRIMITIVES.contains(&&self.typ[..]) => {
                // primitive without endian (e.g. u8)
                let call: TokenStream = syn::parse_str(&format!("_io.write_{}", self.typ)[..]).unwrap();

                let code = match &meta.endian {
                    _ if self.typ == "u8" => quote!( #call ),
                    Endian::Big => quote!( #call::<byteorder::BigEndian> ),
                    Endian::Little => quote!( #call::<byteorder::LittleEndian> ),
                    Endian::Runtime(cond) => quote!(
                        match #cond {
                            Endian::Big => #call::<byteorder::BigEndian>,
                            Endian::Little => #call::<byteorder::BigLittle>,
                        }
                    )
                };

                let typ: TokenStream = syn::parse_str(&self.typ).unwrap();
                quote!( (#[inline] |attr: &#typ| #code(*attr)) )
            },
            Type::Primitive(_) if self.typ == "str" => {
                let enc = match self.encoding.as_ref().unwrap_or(&meta.encoding) {
                    Encoding::Runtime(enc) => enc.clone(),
                    Encoding::Fixed(enc) => {
                        let enc = format!("{:?}", enc);
                        let enc: TokenStream = syn::parse_str(&enc[..]).unwrap();
                        quote!(#enc) // convert to str token
                    }
                };

                quote!( (|s| -> std::io::Result<()> {
                    encode_string(_io, s, &#enc)
                }))
            },
            Type::Custom(_) => {
                // user-defined struct (e.g. super.header)
                let write_fn = TypeSpec::build_function_name("write", &parents, &Some(root));
                let typ = typ.absolute_final_path();

                quote!(
                    (#[inline] |attr: &#typ| -> std::io::Result<()> { attr.#write_fn(_io, &_new_parents, _new_root, _meta, _ctx) } )
                )
            },
            _ => unimplemented!("Attribute::write_call(): type '{}' unknown", self.typ),
        };

        let attr: TokenStream = syn::parse_str(&self.id).unwrap();

        let write_scalar = if self.contents.is_empty() {
            write_scalar
        } else {
            let contents: TokenStream;
            contents = syn::parse_str(&format!("&{:?}", self.contents)[..]).unwrap();
            quote!( (#[inline] |_attr| -> std::io::Result<()> { _io.write_all(#contents) } ) )
        };

        let write_compound = match &self.repeat {
            Repeat::NoRepeat => quote!( (#write_scalar(&attr)?) ),
            _ => quote!(
                for _el in attr {
                    #write_scalar(_el)?
                }
            ),
        };

        let code = if let Some(cond) = &self.cond {
            quote!(
                if let Some(attr) = &self.#attr {
                    assert!(#cond);
                    #write_compound
                } else {
                    assert!(!(#cond));
                }
            )
        } else {
            quote!(
                {
                    let attr = &self.#attr;
                    #write_compound
                }
            )
        };

        quote!( try { #code } )
    }

}

impl Instance {
    pub fn new<T: Into<String>, U: Into<String>>(
        id: T,
        pos: usize,
        value: Option<TokenStream>,
        typ: U,
        repeat: Repeat,
        cond: Option<TokenStream>,
        contents: Vec<u8>,
        enc: Option<Encoding>,
        enum_: Option<String>,
        size_props: Option<SizeProperties>) -> Self
    {
        Self {
            pos,
            value,
            attr: Attribute::new(id, typ, repeat, cond, contents, enc, enum_, size_props),
        }
    }

    pub fn from_attr(attr: Attribute,
        pos: usize,
        value: Option<TokenStream>) -> Self
    {
        Self {
            pos,
            value,
            attr,
        }
    }
}
