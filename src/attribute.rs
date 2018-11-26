use proc_macro2::TokenStream;

use crate::type_spec::TypeSpec;

pub struct Attribute {
    pub id: String,
    pub typ: String,

    //parent: &'a TypeSpec<'a>,
}


impl Attribute {
    pub fn name(&self) -> TokenStream {
        syn::parse_str(&self.id.clone()).unwrap()
    }

    pub fn read_final_struct_call<'a>(&self,
        typ: &TypeSpec<'a>,
        parent_precursors: Vec<TokenStream>,
        root_precursor: TokenStream) -> TokenStream
    {
        let typ = typ.absolute_final_path();
        let read_fn = TypeSpec::build_function_name("read", parent_precursors, Some(root_precursor));

        quote!(
            #typ :: #read_fn (_input, &_new_parents, _new_root, _meta, _ctx)
        )
    }
/*
    pub fn read_impl<'a>(&self,
        typ: &TypeSpec<'a>,
        parent_precursors: Vec<TokenStream>,
        root_precursor: TokenStream) -> TokenStream
    {
        typ.impl_final_read(parent_precursors, Some(root_precursor))
    }
*/
}
