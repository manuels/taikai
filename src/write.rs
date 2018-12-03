use proc_macro2::TokenStream;

use itertools::Itertools;

use crate::type_spec::TypeSpec;
use crate::types::Meta;

impl TypeSpec {
    pub fn impl_final_write(&self,
        parents: &[TokenStream],
        root: &Option<TokenStream>,
        meta: &Meta) -> TokenStream
    {
        let typ = self.absolute_final_path();
        let new_root = if root.is_some() { quote!{ _root } } else { quote!{ self } };
        let new_root_ty = root.clone().unwrap_or(typ.clone());
        let root_ty = root.clone().unwrap_or_else(|| quote!( () ));
        let write_fn = Self::build_function_name("write", &parents, &root);

        let mut new_parents = parents.to_vec();
        new_parents.insert(0, typ.clone());

        let parents_ty = parents.iter();
        let parents_ty = quote! { (#(& #parents_ty, )*) };

        let impl_attr_write = self.seq.iter()
            .map(|attr| attr.resolve_scalar_type(self))
            .unique()
            .map(|attr_typ| attr_typ.impl_final_write(&new_parents, &Some(new_root_ty.clone()), meta));

        // TODO: call write_call for each type only once, not for each attribute!
        let write_attr_call = self.seq.iter().map(|attr| {
            let attr_typ = attr.resolve_scalar_type(self);
            attr.write_call(&attr_typ, &new_parents, new_root_ty.clone(), meta)
        });

        quote!(
            #(#impl_attr_write)*

            impl #typ {
                pub fn #write_fn<T: std::io::Write>(&self, _io: &mut T, _parents: &#parents_ty, _root: &#root_ty, _meta: &Meta, _ctx: &Context) -> std::io::Result<()> {
                    let _new_root = #new_root;
                    let _new_parents = _parents.prepend(self);

                    #(
                        let __r: std::io::Result<()> = #write_attr_call;
                        __r?;
                    )*

                    Ok(())
                }
            }
        )
    }
}
