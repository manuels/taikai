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
}

impl Attribute {
    pub fn new<T: Into<String>, U: Into<String>>
        (id: T, typ: U, repeat: Repeat, cond: Option<TokenStream>) -> Self
    {
        Self {
            id: id.into(),
            typ: typ.into(),
            repeat,
            cond,
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
        let scalar = self.resolve_scalar_type(&structure);
        let scalar = scalar.absolute_final_path();

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
        typ: Type,
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
                let read_fn = TypeSpec::build_function_name("read", &parent_precursors, Some(root_precursor));

                quote!(
                    apply!(#typ :: #read_fn, &_new_parents, _new_root, _meta, _ctx)
                )
            }
            _ => unimplemented!("Attribute::read_call(): type '{}' unknown", self.typ),
        };

        let read_compound = match &self.repeat {
            Repeat::NoRepeat => read_scalar,
            Repeat::Eos => quote!( many0!(#read_scalar) ),
            //Repeat::Until(cond) => quote!( repeat_while!(#cond, #read_scalar) ),
            Repeat::Until(_) => unimplemented!("Repeat::Until not implemented, yet"),
            Repeat::Expr(expr) => quote!( count!(#read_scalar, #expr) ),
        };

        // TODO: handle _input for macros and structs!

        let read = if let Some(cond) = &self.cond {
            quote!( cond!(#cond, #read_compound) )
        } else {
            read_compound
        };

        quote!{
            do_parse!(_input, _v: #read >> (_v))
        }
    }
}
