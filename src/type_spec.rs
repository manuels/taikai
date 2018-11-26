use std::collections::HashMap;

use heck::CamelCase;
use proc_macro2::TokenStream;
use crate::attribute::Attribute;

// TODO: use MarkedTokenStream instead of TokenStream

#[derive(Clone)]
pub enum Type<'a> {
    Primitive(TokenStream),
    Custom(&'a TypeSpec<'a>),
}

impl<'a> Type<'a> {
    pub fn absolute_final_path(&'a self) -> TokenStream {
        match self {
            Type::Primitive(p) => p.clone(),
            Type::Custom(s) => s.absolute_final_path(),
        }
    }

    pub fn impl_final_read(&'a self,
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>) -> TokenStream
    {
        match self {
            Type::Primitive(_) => quote!(),
            Type::Custom(s) => s.impl_final_read(parent_precursors, root_precursor),
        }
    }

    pub fn impl_precursor_reads(&'a self,
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>) -> Vec<TokenStream>
    {
        match self {
            Type::Primitive(_) => vec![],
            Type::Custom(s) => s.impl_precursor_reads(parent_precursors, root_precursor),
        }
    }
}

pub struct TypeSpec<'a> {
    supa: Option<&'a TypeSpec<'a>>,
    root: Option<&'a TypeSpec<'a>>,

    scope: Vec<TokenStream>,

    id: String,
    types: HashMap<String, TypeSpec<'a>>,
    seq: Vec<Attribute>,
}

impl<'a> TypeSpec<'a> {
    //#[cfg(test)]
    pub fn new(scope: Vec<TokenStream>,
        id: String,
        types: HashMap<String, TypeSpec<'a>>,
        seq: Vec<(&str, &str)>,
        ) -> Self
    {
        let mut typ = TypeSpec {
            supa: None,
            root: None,
            scope,
            id,
            types,
            seq: vec![],
        };

        for (id, attr_typ) in seq {
            let attr = Attribute {
                id: id.to_string(), typ: attr_typ.to_string(),
            };
            typ.seq.push(attr);
        }

        typ
    }
}


impl<'a> TypeSpec<'a> {
    pub fn resolve(&'a self, mut path: Vec<&str>) -> Type<'a> {
        if path.is_empty() {
            return Type::Custom(self);
        }

        if path == vec!["u8"] {
            return Type::Primitive(quote!(u8));
        }

        let orig_path = path.clone();
        match path.remove(0) {
            "super" => Type::Custom(self.supa.unwrap_or_else(|| unreachable!())),
            "root" => Type::Custom(self.root.unwrap_or(self)),
            n => {
                let typ = self.types.get(n);
                let typ = typ.unwrap_or_else(|| panic!("Could not resolve type '{}'!", orig_path.join("::")));
                typ.resolve(path)
            }
        }
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

impl<'a> TypeSpec<'a> {
    pub fn name(&'a self) -> TokenStream {
        syn::parse_str(&self.id.to_camel_case()).unwrap()
    }

    pub fn precursor_name(&'a self, attr: &str) -> TokenStream {
        let name = format!("{}__{}", &self.id, attr).to_camel_case();
        syn::parse_str(&name).unwrap()
    }

    pub fn first_precursor_name(&'a self) -> TokenStream {
        self.precursor_name("__precursor")
    }
}

impl<'a> TypeSpec<'a> {
    pub fn final_struct(&'a self) -> TokenStream {
        let name = self.name();
        let attr = self.seq.iter().map(|a| a.name());
        
        let resolve_type = |a: &Attribute| -> Type<'a> {
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

    pub fn precursor_structs(&'a self) -> Vec<TokenStream> {
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


    pub fn impl_final_read(&'a self,
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


    pub fn define(&'a self) -> TokenStream
    {
        let final_struct = self.final_struct();
        let precursor_structs = self.precursor_structs();
        let subtypes = self.types.iter().map(|(_, t)| t.define());

        quote!(
            #[derive(PartialEq, Debug)]
            #final_struct
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


    pub fn impl_precursor_reads(&'a self,
        parent_precursors: Vec<TokenStream>,
        root_precursor: Option<TokenStream>) -> Vec<TokenStream>
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
            let attr_read_call = attr.read_final_struct_call(attr_typ.clone(), new_parent_precursors.clone(), new_root_precursor.clone());
            let attr_impl_final = attr_typ.impl_final_read(new_parent_precursors.clone(), Some(new_root_precursor.clone()));
            let attr_impl_precursors = attr_typ.impl_precursor_reads(new_parent_precursors, Some(new_root_precursor));

            let read_fn = Self::build_function_name("read", parent_precursors.clone(), root_precursor.clone());

            let attributes1 = previous_attributes.iter();
            let attributes2 = previous_attributes.iter();
            let attributes3 = previous_attributes.iter();

            reads.push(quote!(
                #attr_impl_final
                #(#attr_impl_precursors)*

                impl #prev_name {
                    #[inline]
                    pub fn #read_fn<'a>(self, _input: &'a [u8], _parents: &#parents_ty, _root: &#root_ty, _meta: &Meta, _ctx: &Context) -> IoResult<'a, #next_name> {
                        let _new_root = #new_root;
                        let _new_parents = _parents.prepend(&self);
                        #(let #attributes1 = self.#attributes2;)*
                        let (_input, #attr_name) = #attr_read_call?;
                        
                        Ok((_input, #next_name {
                            #(#attributes3, )*
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
