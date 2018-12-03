use proc_macro2::TokenStream;

use crate::type_spec::TypeSpec;
use crate::types::Meta;

impl TypeSpec {
    pub fn impl_final_read(&self,
        parent_precursors: &[TokenStream],
        root_precursor: &Option<TokenStream>) -> TokenStream
    {
        let typ = self.absolute_final_path();
        let read_fn = Self::build_function_name("read", &parent_precursors, &root_precursor);

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

    pub fn impl_precursor_reads(&self,
        parent_precursors: &[TokenStream],
        root_precursor: &Option<TokenStream>,
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
            let new_root_precursor = root_precursor.clone().unwrap_or_else(|| prev_name.clone());
            let some_new_root_precursor = Some(new_root_precursor.clone());
            let mut new_parent_precursors = parent_precursors.to_vec();
            new_parent_precursors.insert(0, prev_name.clone());

            // Prepare read() calls and their _parents/_root-dependent implementation
            let attr_name = attr.name();
            let attr_str_name = format!("{}", attr.name());
            let attr_typ = attr.resolve_scalar_type(self);
            let attr_read_call = attr.read_call(&attr_typ, &new_parent_precursors, new_root_precursor, meta);
            let attr_impl_final = attr_typ.impl_final_read(&new_parent_precursors, &some_new_root_precursor);
            let attr_impl_precursors = attr_typ.impl_precursor_reads(&new_parent_precursors, &some_new_root_precursor, meta);

            let read_fn = Self::build_function_name("read", &parent_precursors, &root_precursor);

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

                        match #attr_read_call {
                            Ok((_input, #attr_name)) => {
                                debug!("{} = {:#?}", #attr_str_name, #attr_name);

                                Ok((_input, #next_name {
                                    #(#attributes3: self.#attributes4, )*
                                    #attr_name
                                }))
                            }
                            Err(err) => {
                                warn!("Failed to read {}", #attr_str_name);
                                Err(err)
                            }
                        }
                    }
                }
            ));

            previous_attributes.push(attr_name.clone());
        }

        reads
    }
}