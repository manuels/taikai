use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use heck::CamelCase;
use proc_macro2::TokenStream;
use crate::attribute::Attribute;
use crate::attribute::Instance;

// TODO: use MarkedTokenStream instead of TokenStream

// Compile-time Endian
#[derive(Debug, Clone)]
pub enum Endian {
    Big,
    Little,
    Runtime(TokenStream),
}

// Compile-time Meta
#[derive(Debug, Clone)]
pub struct Meta {
    pub endian: Endian,
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

pub struct TypeSpec {
    supa: Option<Rc<RefCell<TypeSpec>>>,
    root: Option<Rc<RefCell<TypeSpec>>>,

    scope: Vec<TokenStream>,

    id: String,
    types: HashMap<String, Rc<RefCell<TypeSpec>>>,
    pub seq: Vec<Attribute>,
    instances: HashMap<String, Instance>,
}

impl PartialEq for TypeSpec {
    fn eq(&self, other: &Self) -> bool {
        let self_scope = format!("{:?}", self.scope);
        let other_scope = format!("{:?}", other.scope);

        self.id == other.id && self_scope == other_scope
    } 
}

impl Eq for TypeSpec {}

impl std::hash::Hash for TypeSpec {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let scope = format!("{:?}", self.scope);

        self.id.hash(state);
        scope.hash(state);
    }
}

impl TypeSpec {
    //#[cfg(test)]
    pub fn new(scope: Vec<TokenStream>,
        id: String,
        types: HashMap<String, Rc<RefCell<TypeSpec>>>,
        seq: Vec<Attribute>,
        instances: HashMap<String, Instance>,
        ) -> Rc<RefCell<Self>>
    {
        let typ = TypeSpec {
            supa: None,
            root: None,
            scope,
            id,
            types,
            seq,
            instances,
        };

        let typ = Rc::new(RefCell::new(typ));

        for (_id, subtype) in typ.borrow().types.iter() {
            let mut subtype = subtype.borrow_mut();
            subtype.root.replace(typ.borrow().root.clone().unwrap_or_else(|| Rc::clone(&typ)));
            subtype.supa.replace(Rc::clone(&typ));

            subtype.scope = typ.borrow().scope.clone();
            subtype.scope.push(quote!(__subtypes));
        }

        typ
    }
}


impl TypeSpec {
    pub fn resolve(&self, mut path: Vec<&str>) -> Type {
        let orig_path = path.clone();

        if path.is_empty() {
            panic!("Could not resolve type in '{}': type is an empty string!", self.name());
        }

        let first = path.remove(0);

        if path.is_empty() {
            if let Some(typ) = Type::to_primitive(first) {
                return typ;
            }
        }

        if first == "root" {
            if let Some(root) = &self.root {
                return root.as_ref().borrow().resolve(path);
            } else {
                return self.resolve(path);
            }
        } else if first == "super" {
            if let Some(supa) = &self.supa {
                return supa.as_ref().borrow().resolve(path);
            } else {
                panic!("Could not resolve type '{}' in '{}': super does not exist!", orig_path.join("."), self.name());
            }
        }

        let typ = self.types.get(first);
        let typ = typ.unwrap_or_else(|| panic!("Could not resolve type '{}' in '{}'!", orig_path.join("."), self.name()));

        let mut typ = Rc::clone(typ);

        for el in path {
            typ = {
                let this = typ.as_ref().borrow();
                match this.types.get(el) {
                    None => panic!("Could not resolve type '{}' in '{}' (at {})!", orig_path.join("."), self.name(), el),
                    Some(t) => Rc::clone(t),
                }
            };
        }

        Type::Custom(typ)
    }

    pub fn absolute_final_path(&self) -> TokenStream {
        let path = self.scope.iter();
        let name = self.name();

        quote! {
            #(#path::)* #name
        }
    }

    pub fn absolute_precursor_path(&self, attr: &str) -> TokenStream {
        let path = self.scope.iter();
        let name = self.precursor_name(attr);

        quote! {
            #(#path::)* __precursors:: #name
        }
    }

    pub fn absolute_first_precursor_path(&self) -> TokenStream {
        let path = self.scope.iter();
        let name = self.first_precursor_name();

        quote! {
            #(#path::)* __precursors:: #name
        }
    }
}

impl TypeSpec {
    pub fn name(&self) -> TokenStream {
        syn::parse_str(&self.id.to_camel_case()).unwrap()
    }

    pub fn precursor_name(&self, attr: &str) -> TokenStream {
        let name = format!("{}__{}", &self.id, attr).to_camel_case();
        syn::parse_str(&name).unwrap()
    }

    pub fn first_precursor_name(&self) -> TokenStream {
        self.precursor_name("__precursor")
    }
}

impl TypeSpec {
    pub fn final_struct(&self) -> TokenStream {
        let name = self.name();
        let attr = self.seq.iter().map(|a| a.name());
        
        let resolve_type = |a: &Attribute| -> TokenStream {
            a.absolute_path_of_compound_type(self)
        };

        let attr_type = self.seq.iter()
            .map(resolve_type);

        let structure = quote!(
            pub struct #name {
                #(pub #attr: #attr_type,)*
            }
        );

        structure
    }

    pub fn precursor_structs(&self) -> Vec<TokenStream> {
        let name = self.first_precursor_name();

        let mut structs = vec![quote!(
            pub struct #name {}
            impl #name {
                pub fn new() -> #name {
                    #name {}
                }
            }
        )];

        let mut pairs = quote!();
        for attr in self.seq.iter() {
            let key = attr.name();
            let value = attr.absolute_path_of_compound_type(&self);
            pairs = quote!( #pairs pub #key: #value, );

            let name = self.precursor_name(&attr.id);

            structs.push(quote!(
                pub struct #name {
                    #pairs
                }
            ));
        }

        structs
    }

    pub fn build_function_name(func: &str,
        parent_precursors: &[TokenStream],
        root_precursor: &Option<TokenStream>) -> TokenStream
    {
        let tokenstream2str = |t: &TokenStream| t.to_string().replace(" :: ", "__");

        let root = if let Some(r) = root_precursor { tokenstream2str(&r) } else { "None".to_string() };
        let parents: Vec<_> = parent_precursors.iter().map(tokenstream2str).collect();
        let parents = parents[..].join("__");
        let s = format!("{}___{}___{}", func, parents, root);

        syn::parse_str(&s).unwrap()
    }


    pub fn define(types: &[Rc<RefCell<TypeSpec>>]) -> TokenStream
    {
        let final_struct = types.iter().map(|t| t.borrow().final_struct());
        let precursor_structs = types.iter().map(|t| t.borrow().precursor_structs()).flatten();
        let subtypes: Vec<_> = types.iter()
            .map(|t| {
                let v: Vec<_> = t.borrow().types.values().map(|t| Rc::clone(t)).collect();
                v
            })
            .flatten()
            .collect();
        let subtypes = if subtypes.is_empty() {
            quote!()
        } else {
            Self::define(&subtypes)
        };

        quote!(
            #(
                #[derive(PartialEq, Debug)]
                #final_struct
            )*

            pub mod __precursors {
                #(
                        #[derive(PartialEq, Debug)]
                        #precursor_structs
                )*
            }

            pub mod __subtypes {
                #(#subtypes)*
            }
        )
    }
}
