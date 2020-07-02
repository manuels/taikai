#![recursion_limit = "1024"]
#![allow(dead_code)]
#![feature(proc_macro_span)]
#![feature(try_blocks)]
//#![feature(trace_macros)] trace_macros!(true);

#[macro_use] extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;
#[macro_use] extern crate syn;
extern crate heck;

#[macro_use] extern crate failure;
extern crate itertools;

extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_yaml;

mod types;
mod type_spec;
mod attribute;
mod read;
mod write;
mod parser;
mod enums;

use std::rc::Rc;

use syn::parse::Parser;
use syn::punctuated::Punctuated;

use failure::ResultExt;

use crate::attribute::Repeat;
use crate::attribute::Attribute;
use crate::type_spec::TypeSpec;
use crate::types::Meta;
use crate::types::Endian;
use crate::types::Encoding;
use crate::parser::parse;

/*
    TODO
    - switch-on types
    - instances - pos
    - write repeats
    - write instances
    - enums
    - flags
    - process
    - bit-sized ints
    - repeat-until
    - _io
*/

#[proc_macro]
pub fn taikai_from_str(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let res: Result<proc_macro::TokenStream, failure::Error> = try {
        let parser = Punctuated::<syn::Expr, Token![,]>::parse_separated_nonempty;
        let mut args = parser.parse(input).context("Error parsing macro arguments")?;

        let path = args.pop().ok_or(format_err!("Missing path to yaml file as macro argument"))?;
        let scope = args.pop().ok_or(format_err!("Missing module path as macro argument"))?.into_value();

        let code = quote!( taikai_from_str2!(#scope, #path); );
        code.into()
    };
    res.unwrap()
}

#[proc_macro]
pub fn taikai_from_file(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let res: Result<proc_macro::TokenStream, failure::Error> = try {
        let parser = Punctuated::<syn::Expr, Token![,]>::parse_separated_nonempty;
        let mut args = parser.parse(input).context("Error parsing macro arguments")?;

        let path = args.pop().ok_or(format_err!("Missing path to yaml file as macro argument"))?;
        let scope = args.pop().ok_or(format_err!("Missing module path as macro argument"))?.into_value();

        let path: syn::LitStr = syn::parse2(quote!(#path)).context("Could not parse yaml path")?;
        let path = path.value();
        let path = std::path::Path::new(&path);

        let mut p = proc_macro::Span::call_site().source_file().path();
        let path = if path.is_relative() {
            p.pop();
            p.push(path);
            p.as_path()
        } else {
            path
        };

        use std::io::Read;
        let mut f = std::fs::File::open(path).context("Error opening yaml file")?;
        let mut yaml = String::new();
        f.read_to_string(&mut yaml).context("Error reading yaml file")?;

        let code = quote!( taikai_from_str2!(#scope, #yaml); );
        code.into()
    };
    res.unwrap()
}

#[proc_macro]
pub fn taikai_from_str2(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let res: Result<proc_macro::TokenStream, failure::Error> = try {
        use proc_macro2::TokenStream;
        let runtime: TokenStream = syn::parse_str(include_str!("runtime.rs")).context("Error reading taikai runtime")?;

        let parser = Punctuated::<syn::Expr, Token![,]>::parse_separated_nonempty;
        let mut args = parser.parse(input).context("Error parsing macro arguments")?;

        let yaml = args.pop().ok_or(format_err!("Missing yaml data as macro argument"))?;
        let scope = args.pop().ok_or(format_err!("Missing module path as macro argument"))?.into_value();

        let yaml: syn::LitStr = syn::parse2(quote!(#yaml)).context("Macro argument for yaml data should be a string")?;

        let scope: syn::Path = syn::parse2(quote!(#scope)).context("Macro argument for module path should be a module path")?;
        let scope: Vec<_> = scope.segments.iter().map(|s| s.ident.to_string()).collect();

        let (meta, ctx, typ) = parse(&scope, &yaml.value());

        let context = ctx.borrow().final_struct();
        let definition = TypeSpec::define(&[Rc::clone(&typ)]);

        let typ = typ.borrow();
        let root = typ.absolute_final_path();
        let precursor_impls = typ.impl_precursor_reads(&[], &None, &meta);
        let final_read = typ.impl_final_read(&[], &None);
        let final_write = typ.impl_final_write(&[], &None, &meta);

        let code = quote!(
            #runtime

            #context
            #definition

            #(#precursor_impls)*
            #final_read
            #final_write

            impl #root {
                pub fn read<'a>(_input: &'a [u8], _ctx: &Context) -> IoResult<'a, Self> {
                    let _meta = #meta;
                    Self::read______none(_input, &(), &(), &_meta, _ctx)
                }

                pub fn write<T: std::io::Write>(&self, _io: &mut T, _ctx: &Context) -> std::io::Result<()> {
                    let _meta = #meta;
                    self.write______none(_io, &(), &(), &_meta, _ctx)
                }
            }
        );

        //println!("{}", code);

        code.into()
    };
    res.unwrap()
}

#[proc_macro]
pub fn test_simple(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use std::collections::HashMap;

    use proc_macro2::TokenStream;
    let runtime: TokenStream = syn::parse_str(include_str!("runtime.rs")).unwrap();

    let meta = Meta {
        endian: Endian::Big,
        encoding: Encoding::Fixed(b"utf-8".to_vec()),
    };

    let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_simple), quote!(__subtypes)],
        "bar".into(),
        HashMap::new(),
        vec![Attribute::new("i", "u8", Repeat::NoRepeat, None, vec![], None, None, None)],
        HashMap::new(),
        HashMap::new());
    let mut subtypes = HashMap::new();
    subtypes.insert("bar".into(), subtyp);

    let seq = vec![
        Attribute::new("i", "u8", Repeat::NoRepeat, None, vec![], None, None, None),
        Attribute::new("baz", "bar", Repeat::NoRepeat, None, vec![], None, None, None),
        Attribute::new("j", "u8", Repeat::NoRepeat, None, vec![], None, None, None)
    ];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_simple)],
        "root".into(),
        subtypes,
        seq,
        HashMap::new(),
        HashMap::new());

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
            pub fn read<'a>(_input: &'a [u8], _ctx: &Context) -> IoResult<'a, Self> {
                let _meta = #meta;
                Self::read______none(_input, &(), &(), &_meta, _ctx)
            }
        }
    );

    //println!("{}", code);

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
        encoding: Encoding::Fixed(b"utf-8".to_vec()),
    };

    let mut subtypes = HashMap::new();
    {
        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "header".into(),
            HashMap::new(),
            vec![Attribute::new("i", "u8", Repeat::NoRepeat, None, vec![], None, None, None)],
            HashMap::new(),
            HashMap::new());
        subtypes.insert("header".into(), subtyp);

        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "body1".into(),
            HashMap::new(),
            vec![Attribute::new("foo", "super.header", Repeat::NoRepeat, None, vec![], None, None, None)],
            HashMap::new(),
            HashMap::new());
        subtypes.insert("body1".into(), subtyp);

        let mut subtypes2 = HashMap::new();
        {
            let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
                "header".into(),
                HashMap::new(),
                vec![Attribute::new("j", "u8", Repeat::NoRepeat, None, vec![], None, None, None)],
                HashMap::new(),
                HashMap::new());
            subtypes2.insert("header".into(), subtyp);
        }
        let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_resolve), quote!(__subtypes)],
            "body2".into(),
            subtypes2,
            vec![Attribute::new("foo", "header", Repeat::NoRepeat, None, vec![], None, None, None)],
            HashMap::new(),
            HashMap::new());
        subtypes.insert("body2".into(), subtyp);
    }

    let seq = vec![
        Attribute::new("foo", "header", Repeat::NoRepeat, None, vec![], None, None, None),
        Attribute::new("bar", "body1", Repeat::NoRepeat, None, vec![], None, None, None),
        Attribute::new("baz", "body2", Repeat::NoRepeat, None, vec![], None, None, None)
    ];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_resolve)],
        "root".into(),
        subtypes,
        seq,
        HashMap::new(),
        HashMap::new());
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
            pub fn read<'a>(_input: &'a [u8], _ctx: &Context) -> IoResult<'a, Self> {
                let _meta = #meta;
                Self::read______none(_input, &(), &(), &_meta, _ctx)
            }
        }
    );

    //println!("{}", code);

    code.into()
}

#[proc_macro]
pub fn test_compound(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use std::collections::HashMap;

    use proc_macro2::TokenStream;
    let runtime: TokenStream = syn::parse_str(include_str!("runtime.rs")).unwrap();

    let meta = Meta {
        endian: Endian::Big,
        encoding: Encoding::Fixed(b"utf-8".to_vec()),
    };

    let subtyp = TypeSpec::new(vec![quote!(crate), quote!(test_compound), quote!(__subtypes)],
        "bar".into(),
        HashMap::new(),
        vec![Attribute::new("i", "u8", Repeat::Expr(quote!(3)), None, vec![], None, None, None)],
        HashMap::new(),
        HashMap::new());
    let mut subtypes = HashMap::new();
    subtypes.insert("bar".into(), subtyp);

    let seq = vec![
        Attribute::new("i", "u16be", Repeat::NoRepeat, None, vec![], None, None, None),
        Attribute::new("j", "u16le", Repeat::NoRepeat, None, vec![], None, None, None),
        Attribute::new("baz", "bar", Repeat::NoRepeat, Some(quote!(self.i == 0x0102)), vec![], None, None, None),
        Attribute::new("k", "u8le", Repeat::Expr(quote!(1)), Some(quote!(self.i == 0x9999)), vec![], None, None, None),
        Attribute::new("l", "u8le", Repeat::NoRepeat, Some(quote!(true)), vec![], None, None, None),
        Attribute::new("bytes", "u8", Repeat::Expr(quote!(2)), Some(quote!(true)), b"abc".to_vec(), None, None, None),
    ];

    let typ = TypeSpec::new(vec![quote!(crate), quote!(test_compound)],
        "root".into(),
        subtypes,
        seq,
        HashMap::new(),
        HashMap::new());

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
            pub fn read<'a>(_input: &'a [u8], _ctx: &Context) -> IoResult<'a, Self> {
                let _meta = #meta;
                Self::read______none(_input, &(), &(), &_meta, _ctx)
            }
        }
    );

    //println!("{}", code);

    code.into()
}
