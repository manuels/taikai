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

    pub fn read_final_struct_call<'a>(&self,
        typ: Type<'a>,
        parent_precursors: Vec<TokenStream>,
        root_precursor: TokenStream) -> TokenStream
    {
        match typ {
            Type::Primitive(_) if self.typ == "u8" => quote!( nom::be_u8(_input) ),
            Type::Custom(typ) => {
                let typ = typ.absolute_final_path();
                let read_fn = TypeSpec::build_function_name("read", parent_precursors, Some(root_precursor));

                quote!(
                    #typ :: #read_fn (_input, &_new_parents, _new_root, _meta, _ctx)
                )
            }
            _ => unimplemented!("Attribute::read_final_struct_call(): type '{}'", self.typ),
        }
    }
}
