#![recursion_limit = "1024"]
#![allow(dead_code)]
//#![feature(trace_macros)] trace_macros!(true);

#[macro_use]
extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate heck;

mod type_spec;
mod attribute;

use crate::type_spec::TypeSpec;

#[proc_macro]
pub fn test_simple(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use std::collections::HashMap;

    use proc_macro2::TokenStream;
    let runtime: TokenStream = syn::parse_str(include_str!("runtime.rs")).unwrap();

    let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_simple), quote!(__subtypes)],
        "bar".into(),
        HashMap::new(),
        vec![("i", "u8")]);
    let mut subtypes = HashMap::new();
    subtypes.insert("bar".into(), subtyp);

    let seq = vec![("i", "u8"), ("baz", "bar"), ("j", "u8")];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_simple)],
        "foo".into(),
        subtypes,
        seq);

    let root = typ.absolute_final_path();
    let definition = typ.define();
    let precursor_impls = typ.impl_precursor_reads(vec![], None);
    let final_impl = typ.impl_final_read(vec![], None);
    
    let code = quote!(
        #runtime
        
        #definition

        #(#precursor_impls)*
        #final_impl

        impl #root {
            pub fn read<'a>(_input: &'a [u8], _meta: &Meta, _ctx: &Context) -> IoResult<'a, Self> {
                Self::read______None(_input, &(), &(), _meta, _ctx)
            }
        }
    );
    
    println!("{}", code);

    code.into()
}
