use proc_macro2::TokenStream;

use crate::type_spec::TypeSpec;
use crate::type_spec::Type;

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
        root_precursor: TokenStream) -> TokenStream
    {
        match typ {
            Type::Primitive(_) if self.typ == "u8"  => quote!( nom::be_u8(_input) ),
            Type::Primitive(_) if self.typ == "u16" => quote!( nom::be_u16(_input) ),
            Type::Primitive(_) if self.typ == "u32" => quote!( nom::be_u32(_input) ),
            Type::Primitive(_) if self.typ == "u64" => quote!( nom::be_u64(_input) ),
            Type::Primitive(_) if self.typ == "u128" => quote!( nom::be_u128(_input) ),
            Type::Primitive(_) if self.typ == "i8"  => quote!( nom::be_i8(_input) ),
            Type::Primitive(_) if self.typ == "i16" => quote!( nom::be_i16(_input) ),
            Type::Primitive(_) if self.typ == "i32" => quote!( nom::be_i32(_input) ),
            Type::Primitive(_) if self.typ == "i64" => quote!( nom::be_i64(_input) ),
            Type::Primitive(_) if self.typ == "i128" => quote!( nom::be_i128(_input) ),
            Type::Custom(typ) => {
                let typ = typ.as_ref().borrow().absolute_final_path();
                let read_fn = TypeSpec::build_function_name("read", parent_precursors, Some(root_precursor));

                quote!(
                    #typ :: #read_fn (_input, &_new_parents, _new_root, _meta, _ctx)
                )
            }
            _ => unimplemented!("Attribute::read_final_struct_call(): type '{}'", self.typ),
        }
    }
}
