#![recursion_limit = "1024"]
#![allow(dead_code)]
#![feature(bind_by_move_pattern_guards)]
//#![feature(trace_macros)] trace_macros!(true);

#[macro_use]
extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate heck;

mod type_spec;
mod attribute;

use std::rc::Rc;

use crate::type_spec::TypeSpec;
use crate::type_spec::Meta;
use crate::type_spec::Endian;

#[proc_macro]
pub fn test_simple(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use std::collections::HashMap;

    use proc_macro2::TokenStream;
    let runtime: TokenStream = syn::parse_str(include_str!("runtime.rs")).unwrap();

    let meta = Meta {
        endian: Endian::Big,
    };

    let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_simple), quote!(__subtypes)],
        "bar".into(),
        HashMap::new(),
        vec![("i", "u8")]);
    let mut subtypes = HashMap::new();
    subtypes.insert("bar".into(), subtyp);

    let seq = vec![("i", "u8be"), ("baz", "bar"), ("j", "u8")];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_simple)],
        "foo".into(),
        subtypes,
        seq);

    let definition = TypeSpec::define(vec![Rc::clone(&typ)]);

    let typ = typ.borrow();
    let root = typ.absolute_final_path();
    let precursor_impls = typ.impl_precursor_reads(vec![], None, &meta);
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

#[proc_macro]
// example from http://doc.kaitai.io/ksy_reference.html#attribute-type
pub fn test_resolve(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use std::collections::HashMap;

    use proc_macro2::TokenStream;
    let runtime: TokenStream = syn::parse_str(include_str!("runtime.rs")).unwrap();

    let meta = Meta {
        endian: Endian::Big,
    };

    let mut subtypes = HashMap::new();
    {
        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "header".into(),
            HashMap::new(),
            vec![("i", "u8")]);
        subtypes.insert("header".into(), subtyp);

        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "body1".into(),
            HashMap::new(),
            vec![("foo", "super.header")]);
        subtypes.insert("body1".into(), subtyp);

        let mut subtypes2 = HashMap::new();
        {
            let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
                "header".into(),
                HashMap::new(),
                vec![("j", "u8")]);
            subtypes2.insert("header".into(), subtyp);
        }
        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "body2".into(),
            subtypes2,
            vec![("foo", "header")]);
        subtypes.insert("body2".into(), subtyp);
    }

    let seq = vec![("foo", "header"), ("bar", "body1"), ("baz", "body2")];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_resolve)],
        "root".into(),
        subtypes,
        seq);
    let definition = TypeSpec::define(vec![Rc::clone(&typ)]);

    let typ = typ.borrow();
    let root = typ.absolute_final_path();
    let precursor_impls = typ.impl_precursor_reads(vec![], None, &meta);
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
