use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use heck::CamelCase;
use proc_macro2::TokenStream;

use crate::types::Type;

use crate::attribute::Attribute;
use crate::attribute::Instance;

pub struct TypeSpec {
    supa: Option<Rc<RefCell<TypeSpec>>>,
    root: Option<Rc<RefCell<TypeSpec>>>,

    scope: Vec<TokenStream>,

    id: String,
    types: HashMap<String, Rc<RefCell<TypeSpec>>>,
    pub seq: Vec<Attribute>,
    pub instances: HashMap<String, Instance>,
    pub enums: HashMap<String, HashMap<String, String>>,
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
        enums: HashMap<String, HashMap<String, String>>,
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
            enums,
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

        let impl_instances = self.impl_instances();
        let enums = self.enums.iter().map(|(id, e)| Self::define_enum(id, e));

        let structure = quote!(
            pub struct #name {
                #(pub #attr: #attr_type,)*
            }

            #(#enums)*

            impl #name {
                #(#impl_instances)*
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

    fn impl_instances(&self) -> Vec<TokenStream> {
        let mut instances = vec![];

        for (id, inst) in self.instances.iter() {
            let id: TokenStream = syn::parse_str(&id).unwrap();
            let typ: TokenStream = syn::parse_str(&inst.attr.typ).unwrap();

            if let Some(value) = &inst.value {
                instances.push(quote!(
                    pub fn #id(&self) -> #typ {
                        #value
                    }
                ))
            } else {
                unimplemented!()
            }
        }

        instances
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
        let s = s.to_lowercase();

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

    pub fn define_enum(id: &String, e: &HashMap<String, String>) -> TokenStream {
        unimplemented!()
    }
}
