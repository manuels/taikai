use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use heck::CamelCase;
use proc_macro2::TokenStream;
use crate::attribute::Attribute;

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
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>) -> TokenStream
    {
        match self {
            Type::Primitive(_) => quote!(),
            Type::Custom(s) => s.as_ref().borrow().impl_final_read(parent_precursors, root_precursor),
        }
    }

    pub fn impl_precursor_reads(&self,
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>,
        meta: &Meta) -> Vec<TokenStream>
    {
        match self {
            Type::Primitive(_) => vec![],
            Type::Custom(s) => s.as_ref().borrow().impl_precursor_reads(parent_precursors, root_precursor, meta),
        }
    }

    pub fn as_primitive(mut name: &str) -> Option<Type> {
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

pub struct TypeSpec {
    supa: Option<Rc<RefCell<TypeSpec>>>,
    root: Option<Rc<RefCell<TypeSpec>>>,

    scope: Vec<TokenStream>,

    id: String,
    types: HashMap<String, Rc<RefCell<TypeSpec>>>,
    seq: Vec<Attribute>,
}

impl TypeSpec {
    //#[cfg(test)]
    pub fn new(scope: Vec<TokenStream>,
        id: String,
        types: HashMap<String, Rc<RefCell<TypeSpec>>>,
        seq: Vec<(&str, &str)>,
        ) -> Rc<RefCell<Self>>
    {
        let mut typ = TypeSpec {
            supa: None,
            root: None,
            scope,
            id,
            types: types,
            seq: vec![],
        };

        for (id, attr_typ) in seq {
            let attr = Attribute {
                id: id.to_string(), typ: attr_typ.to_string(),
            };
            typ.seq.push(attr);
        }

        let typ = Rc::new(RefCell::new(typ));

        for (_id, subtype) in typ.borrow().types.iter() {
            let mut subtype = subtype.borrow_mut();
            subtype.root.replace(typ.borrow().root.clone().unwrap_or(Rc::clone(&typ)));
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
            if let Some(typ) = Type::as_primitive(first) {
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
                let new_typ = match this.types.get(el) {
                    None => panic!("Could not resolve type '{}' in '{}' (at {})!", orig_path.join("."), self.name(), el),
                    Some(t) => Rc::clone(t),
                };
                new_typ
            };
        }

        return Type::Custom(typ);
    }

    pub fn absolute_final_path(&self) -> TokenStream {
        let path = self.scope.iter();
        let name = self.name();

        quote! {
            #(#path::)* #name
        }
    }

    fn absolute_precursor_path(&self, attr: &str) -> TokenStream {
        let path = self.scope.iter();
        let name = self.precursor_name(attr);

        quote! {
            #(#path::)* __precursors:: #name
        }
    }

    fn absolute_first_precursor_path(&self) -> TokenStream {
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
        
        let resolve_type = |a: &Attribute| -> Type {
            let path = a.typ.split('.').collect();
            self.resolve(path)
        };

        let attr_type = self.seq.iter()
            .map(resolve_type)
            .map(|t| t.absolute_final_path());

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
            let path = attr.typ.split('.').collect();
            let value = self.resolve(path).absolute_final_path();
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
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>) -> TokenStream
    {
        let tokenstream2str = |t: &TokenStream| t.to_string().replace(" :: ", "__");

        let root = if let Some(r) = root_precursor { tokenstream2str(&r) } else { "None".to_string() };
        let parents: Vec<_> = parent_precursors.iter().map(tokenstream2str).collect();
        let parents = parents[..].join("__");
        let s = format!("{}___{}___{}", func, parents, root);

        syn::parse_str(&s).unwrap()
    }


    pub fn impl_final_read(&self,
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>) -> TokenStream
    {
        let typ = self.absolute_final_path();
        let read_fn = Self::build_function_name("read", parent_precursors.clone(), root_precursor.clone());

        let root_ty = root_precursor.clone().unwrap_or_else(|| quote!( () ));

        let parents_ty = parent_precursors.iter();
        let parents_ty = quote! { (#(& #parents_ty, )*) };

        let precursor_ty = self.absolute_first_precursor_path();
        let precursor_read = self.seq.iter().map(|_| read_fn.clone());

        let attributes1 = self.seq.iter().map(|a| a.name());
        let attributes2 = self.seq.iter().map(|a| a.name());

        quote!(
            impl #typ {
                pub fn #read_fn<'a>(_input: &'a [u8], _parents: &#parents_ty, _root: &#root_ty, _meta: &Meta, _ctx: &Context) -> IoResult<'a, #typ> {
                    let _obj = #precursor_ty::new();
                    #( let (_input, _obj) = _obj.#precursor_read(_input, _parents, _root, _meta, _ctx)?; )*

                    Ok((_input, #typ {
                        #(#attributes1: _obj.#attributes2,)*
                    }))
                }
            }
        )
    }


    pub fn define(types: Vec<Rc<RefCell<TypeSpec>>>) -> TokenStream
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
            Self::define(subtypes)
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


    pub fn impl_precursor_reads(&self,
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>,
        meta: &Meta) -> Vec<TokenStream>
    {
        // Create root and parent types
        let root_ty = root_precursor.clone().unwrap_or_else(|| quote!( () ));
        let new_root = if root_precursor.is_some() { quote!{ _root } } else { quote!{ &self } };

        let parents_ty = parent_precursors.iter();
        let parents_ty = quote! { (#(& #parents_ty, )*) };

        // Loop over attribute pairs
        let mut reads = vec![];
        let mut previous_attributes = vec![];

        let iter = vec![self.absolute_first_precursor_path()].into_iter();        
        let precursors1 = iter.chain(self.seq.iter().map(|a| self.absolute_precursor_path(&a.id)).rev().skip(1).rev());
        let precursors2 = self.seq.iter().map(|a| self.absolute_precursor_path(&a.id));

        for (attr, (prev_name, next_name)) in self.seq.iter().zip(precursors1.zip(precursors2)) {
            // Create new root and parent types for subtypes
            let new_root_precursor = root_precursor.clone().unwrap_or(prev_name.clone());
            let mut new_parent_precursors = parent_precursors.clone();
            new_parent_precursors.insert(0, prev_name.clone());

            // Prepare read() calls and their _parents/_root-dependent implementation
            let attr_name = attr.name();
            let attr_typ = self.resolve(attr.typ.split('.').collect());
            let attr_read_call = attr.read_final_struct_call(attr_typ.clone(), new_parent_precursors.clone(), new_root_precursor.clone(), meta);
            let attr_impl_final = attr_typ.impl_final_read(new_parent_precursors.clone(), Some(new_root_precursor.clone()));
            let attr_impl_precursors = attr_typ.impl_precursor_reads(new_parent_precursors, Some(new_root_precursor), meta);

            let read_fn = Self::build_function_name("read", parent_precursors.clone(), root_precursor.clone());

            let attributes1 = previous_attributes.iter();
            let attributes2 = previous_attributes.iter();
            let attributes3 = previous_attributes.iter();
            let attributes4 = previous_attributes.iter();

            reads.push(quote!(
                #attr_impl_final
                #(#attr_impl_precursors)*

                impl #prev_name {
                    #[inline]
                    pub fn #read_fn<'a>(self, _input: &'a [u8], _parents: &#parents_ty, _root: &#root_ty, _meta: &Meta, _ctx: &Context) -> IoResult<'a, #next_name> {
                        let _new_root = #new_root;
                        let _new_parents = _parents.prepend(&self);
                        #(let #attributes1 = &self.#attributes2;)*
                        let (_input, #attr_name) = #attr_read_call?;

                        Ok((_input, #next_name {
                            #(#attributes3: self.#attributes4, )*
                            #attr_name
                        }))
                    }
                }
            ));

            previous_attributes.push(attr_name.clone());
        }

        reads
    }
}
