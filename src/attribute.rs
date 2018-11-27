use proc_macro2::TokenStream;

use crate::type_spec::TypeSpec;
use crate::type_spec::Type;
use crate::type_spec::Meta;
use crate::type_spec::Endian;

pub const PRIMITIVES: &'static [&'static str] = &[
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

pub struct Attribute {
    pub id: String,
    pub typ: String,
}

impl Attribute {
    pub fn name(&self) -> TokenStream {
        syn::parse_str(&self.id.clone()).unwrap()
    }

    pub fn read_final_struct_call(&self,
        typ: Type,
        parent_precursors: Vec<TokenStream>,
        root_precursor: TokenStream,
        meta: &Meta) -> TokenStream
    {
        let primitive_with_endian = |p: &&str| {
            self.typ.starts_with(p) &&
            self.typ.len() == p.len() + 2 &&
            (self.typ.ends_with("le") || self.typ.ends_with("be"))
        };

        match typ {
            Type::Primitive(_) if PRIMITIVES.iter().any(primitive_with_endian) => {
                // primitive with endian (e.g. u8be)
                let (typ, endian) = &self.typ.split_at(self.typ.len() - 2);

                syn::parse_str(&format!("nom::{}_{}(_input)", endian, typ)[..]).unwrap()
            }
            Type::Primitive(_) if PRIMITIVES.contains(&&self.typ[..]) => {
                // primitive without endian (e.g. u8)
                let big = syn::parse_str(&format!("nom::be_{}(_input)", self.typ)[..]).unwrap();
                let little = syn::parse_str(&format!("nom::le_{}(_input)", self.typ)[..]).unwrap();

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
                let read_fn = TypeSpec::build_function_name("read", parent_precursors, Some(root_precursor));

                quote!(
                    #typ :: #read_fn (_input, &_new_parents, _new_root, _meta, _ctx)
                )
            }
            _ => unimplemented!("Attribute::read_final_struct_call(): type '{}' unknown", self.typ),
        }
    }
}
