use proc_macro2::TokenStream;

use crate::type_spec::TypeSpec;
use crate::type_spec::Type;
use crate::type_spec::Meta;
use crate::type_spec::Endian;

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

impl Default for Repeat {
    fn default() -> Self {
        Repeat::NoRepeat
    }
}

#[derive(Default, Debug)]
pub struct Attribute {
    pub id: String,
    pub typ: String,
    pub repeat: Repeat,
    pub cond: Option<TokenStream>,
    pub contents: Vec<u8>
}

pub struct Instance {
    attr: Attribute,
    pos: usize,
    value: Option<TokenStream>,
}

impl Attribute {
    pub fn new<T: Into<String>, U: Into<String>>(
        id: T,
        typ: U,
        repeat: Repeat,
        cond: Option<TokenStream>,
        contents: Vec<u8>) -> Self
    {
        Self {
            id: id.into(),
            typ: typ.into(),
            repeat,
            cond,
            contents,
        }
    }

    pub fn name(&self) -> TokenStream {
        syn::parse_str(&self.id.clone()).unwrap()
    }

    pub fn resolve_scalar_type(&self, structure: &TypeSpec) -> Type {
        let path = self.typ.split('.').collect();
        structure.resolve(path)
    }

    pub fn absolute_path_of_compound_type(&self, structure: &TypeSpec) -> TokenStream {
        let scalar = if self.contents.is_empty() {
            let scalar = self.resolve_scalar_type(&structure);
            scalar.absolute_final_path()
        } else {
            quote!{ () }
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

        let read_scalar = match typ {
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
            Type::Custom(typ) => {
                // user-defined struct (e.g. super.header)
                let typ = typ.as_ref().borrow().absolute_final_path();
                let read_fn = TypeSpec::build_function_name("read", &parent_precursors, &Some(root_precursor));

                quote!(
                    apply!(#typ :: #read_fn, &_new_parents, _new_root, _meta, _ctx)
                )
            }
            _ => unimplemented!("Attribute::read_call(): type '{}' unknown", self.typ),
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
            Repeat::Eos => quote!( many0!(#read_scalar) ),
            //Repeat::Until(cond) => quote!( repeat_while!(#cond, #read_scalar) ),
            Repeat::Until(_) => unimplemented!("Repeat::Until not implemented, yet"),
            Repeat::Expr(expr) => quote!( count!(#read_scalar, #expr) ),
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
                    quote!( (|attr: &#typ| _io.#func(*attr)) )
                } else {
                    quote!( (|attr: &#typ| _io.#func::<#endian>(*attr)) )
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
                quote!( (|attr: &#typ| #code(*attr)) )
            },
            Type::Custom(_) => {
                // user-defined struct (e.g. super.header)
                let write_fn = TypeSpec::build_function_name("write", &parents, &Some(root));
                let typ = typ.absolute_final_path();

                quote!(
                    (|attr: &#typ| -> std::io::Result<()> { attr.#write_fn(_io, &_new_parents, _new_root, _meta, _ctx) } )
                )
            }
            _ => unimplemented!("Attribute::write_call(): type '{}' unknown", self.typ),
        };

        let attr: TokenStream = syn::parse_str(&self.id).unwrap();

        let write_scalar = if self.contents.is_empty() {
            write_scalar
        } else {
            let contents: TokenStream;
            contents = syn::parse_str(&format!("&{:?}", self.contents)[..]).unwrap();
            quote!( (|_attr| -> std::io::Result<()> { _io.write(&#contents) } ) )
        };

        let write_compound = match &self.repeat {
            Repeat::NoRepeat => quote!( #write_scalar(&self.#attr)? ),
            _ => quote!(
                for _el in self.#attr {
                    #write_scalar(_el)?
                }
            ),
        };

        let code = if let Some(cond) = &self.cond {
            quote!(
                if #cond {
                    #write_compound
                }
            )
        } else {
            write_compound
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
        contents: Vec<u8>) -> Self
    {
        Self {
            pos,
            value,
            attr: Attribute::new(id, typ, repeat, cond, contents),
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
