use std::rc::Rc;
use std::cell::RefCell;

use proc_macro2::TokenStream;

use crate::type_spec::TypeSpec;

// TODO: use MarkedTokenStream instead of TokenStream

// Compile-time Endian
#[derive(Debug, Clone)]
pub enum Endian {
    Big,
    Little,
    Runtime(TokenStream),
}

#[derive(Debug, Clone)]
pub enum Encoding {
    Fixed(Vec<u8>),
    Runtime(TokenStream),
}

// Compile-time Meta
#[derive(Debug, Clone)]
pub struct Meta {
    pub endian: Endian,
    pub encoding: Encoding,
}

impl quote::ToTokens for Endian {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Endian::Big => (quote!( Endian::Big )).to_tokens(tokens),
            Endian::Little => (quote!( Endian::Little )).to_tokens(tokens),
            Endian::Runtime(_) => unimplemented!("Endian::Runtime.to_token()")
        }
    }
}

impl quote::ToTokens for Meta {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let endian = &self.endian;
        (quote! {
            Meta {
                endian: #endian,
            }
        }).to_tokens(tokens)
    }
}

// Holds storage information only (no endianness, Attribute does that)!
#[derive(Clone)]
pub enum Type {
    Primitive(TokenStream),
    Custom(Rc<RefCell<TypeSpec>>),
}

impl Type {
    pub fn absolute_final_path(&self) -> TokenStream {
        match self {
            Type::Primitive(p) => p.clone(),
            Type::Custom(s) => s.as_ref().borrow().absolute_final_path(),
        }
    }

    pub fn impl_final_read(&self,
        parent_precursors: &[TokenStream],
        root_precursor: &Option<TokenStream>) -> TokenStream
    {
        match self {
            Type::Primitive(_) => quote!(),
            Type::Custom(s) => s.as_ref().borrow().impl_final_read(parent_precursors, root_precursor),
        }
    }

    pub fn impl_precursor_reads(&self,
        parent_precursors: &[TokenStream],
        root_precursor: &Option<TokenStream>,
        meta: &Meta) -> Vec<TokenStream>
    {
        match self {
            Type::Primitive(_) => vec![],
            Type::Custom(s) => s.as_ref().borrow().impl_precursor_reads(parent_precursors, root_precursor, meta),
        }
    }

    pub fn impl_final_write(&self,
        parents: &[TokenStream],
        root: &Option<TokenStream>,
        meta: &Meta) -> TokenStream
    {
        match self {
            Type::Primitive(_) => quote!(),
            Type::Custom(s) => s.as_ref().borrow().impl_final_write(parents, root, meta),
        }
    }

    pub fn to_primitive(mut name: &str) -> Option<Type> {
        if name.ends_with("le") || name.ends_with("be") {
            name = &name[..name.len() - 2];
        }

        match name {
            "u8"   => Some(Type::Primitive(quote!(u8))),
            "u16"  => Some(Type::Primitive(quote!(u16))),
            "u32"  => Some(Type::Primitive(quote!(u32))),
            "u64"  => Some(Type::Primitive(quote!(u64))),
            "u128" => Some(Type::Primitive(quote!(u128))),
            "i8"   => Some(Type::Primitive(quote!(i8))),
            "i16"  => Some(Type::Primitive(quote!(i16))),
            "i32"  => Some(Type::Primitive(quote!(i32))),
            "i64"  => Some(Type::Primitive(quote!(i64))),
            "i128" => Some(Type::Primitive(quote!(i128))),
            "f32"  => Some(Type::Primitive(quote!(f32))),
            "f64"  => Some(Type::Primitive(quote!(f64))),
            _ => None
        }
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Type::Primitive(p) => format!("{:?}", p).hash(state),
            Type::Custom(t) => t.borrow().hash(state),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Primitive(t1), Type::Primitive(t2)) => {
                format!("{:?}", t1) == format!("{:?}", t2)
            }
            (Type::Custom(t1), Type::Custom(t2)) => {
                *t1.borrow() == *t2.borrow()
            }
            _ => false
        }
    } 
}

impl Eq for Type {}
