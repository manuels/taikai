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

use crate::attribute::Repeat;
use crate::attribute::Attribute;
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
        vec![Attribute::new("i", "u8", Repeat::NoRepeat, None)]);
    let mut subtypes = HashMap::new();
    subtypes.insert("bar".into(), subtyp);

    let seq = vec![
        Attribute::new("i", "u8", Repeat::NoRepeat, None), 
        Attribute::new("baz", "bar", Repeat::NoRepeat, None),
        Attribute::new("j", "u8", Repeat::NoRepeat, None)
    ];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_simple)],
        "root".into(),
        subtypes,
        seq);

    let definition = TypeSpec::define(&[Rc::clone(&typ)]);

    let typ = typ.borrow();
    let root = typ.absolute_final_path();
    let precursor_impls = typ.impl_precursor_reads(&[], &None, &meta);
    let final_impl = typ.impl_final_read(&[], &None);
    
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
pub fn test_compound(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use std::collections::HashMap;

    use proc_macro2::TokenStream;
    let runtime: TokenStream = syn::parse_str(include_str!("runtime.rs")).unwrap();

    let meta = Meta {
        endian: Endian::Big,
    };

    let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_compound), quote!(__subtypes)],
        "bar".into(),
        HashMap::new(),
        vec![Attribute::new("i", "u8", Repeat::Expr(quote!(3)), None)]);
    let mut subtypes = HashMap::new();
    subtypes.insert("bar".into(), subtyp);

    let seq = vec![
        Attribute::new("i", "u16be", Repeat::NoRepeat, None), 
        Attribute::new("j", "u16le", Repeat::NoRepeat, None), 
        Attribute::new("baz", "bar", Repeat::NoRepeat, Some(quote!(self.i == 0x0102))),
        Attribute::new("k", "u8le", Repeat::Expr(quote!(1)), Some(quote!(self.i == 0x9999))),
        Attribute::new("l", "u8le", Repeat::NoRepeat, Some(quote!(true))),
    ];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_compound)],
        "root".into(),
        subtypes,
        seq);

    let definition = TypeSpec::define(&[Rc::clone(&typ)]);

    let typ = typ.borrow();
    let root = typ.absolute_final_path();
    let precursor_impls = typ.impl_precursor_reads(&[], &None, &meta);
    let final_impl = typ.impl_final_read(&[], &None);
    
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
            vec![Attribute::new("i", "u8", Repeat::NoRepeat, None)]);
        subtypes.insert("header".into(), subtyp);

        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "body1".into(),
            HashMap::new(),
            vec![Attribute::new("foo", "super.header", Repeat::NoRepeat, None)]);
        subtypes.insert("body1".into(), subtyp);

        let mut subtypes2 = HashMap::new();
        {
            let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
                "header".into(),
                HashMap::new(),
                vec![Attribute::new("j", "u8", Repeat::NoRepeat, None)]);
            subtypes2.insert("header".into(), subtyp);
        }
        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "body2".into(),
            subtypes2,
            vec![Attribute::new("foo", "header", Repeat::NoRepeat, None)]);
        subtypes.insert("body2".into(), subtyp);
    }

    let seq = vec![
        Attribute::new("foo", "header", Repeat::NoRepeat, None),
        Attribute::new("bar", "body1", Repeat::NoRepeat, None),
        Attribute::new("baz", "body2", Repeat::NoRepeat, None)
    ];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_resolve)],
        "root".into(),
        subtypes,
        seq);
    let definition = TypeSpec::define(&[Rc::clone(&typ)]);

    let typ = typ.borrow();
    let root = typ.absolute_final_path();
    let precursor_impls = typ.impl_precursor_reads(&[], &None, &meta);
    let final_impl = typ.impl_final_read(&[], &None);
    
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
