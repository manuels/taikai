use proc_macro2::TokenStream;
use quote::quote;

use heck::CamelCase;

pub struct Enum {
    pub id: String,
    pub scope: Vec<TokenStream>,
    pub pairs: Vec<(String, String)>
}

impl Enum {
    pub fn name(&self) -> TokenStream {
        syn::parse_str(&self.id.to_camel_case()).unwrap()
    }

    pub fn absolute_final_path(&self) -> TokenStream {
        let path = self.scope.iter();
        let name = self.name();

        quote! {
            #(#path::)* #name
        }
    }

    pub fn variant_name(&self, variant: &str) -> TokenStream {
        let variant = variant.to_camel_case();
        let variant: TokenStream = syn::parse_str(&variant).unwrap();
        let typ = self.name();
        quote!( #typ::#variant )
    }

    pub fn match_from_primitive(&self, read: TokenStream) -> TokenStream
    {
        let pairs: (Vec<TokenStream>, Vec<syn::Expr>);
        pairs = self.pairs.iter().map(|(key, value)| {
            let key = self.variant_name(key);
            let value: syn::Expr = syn::parse_str(&value).unwrap();

            (key, value)
        }).unzip();
        let (key, value) = pairs;

        quote!(
            {
                let _v = match #read {
                    #(#value => Ok(#key), )*
                    _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidData,
                             "Invalid value for enum")),
                }?;

                Ok((_input, _v))
            }
        )
    }

    pub fn match_to_primitive(&self, variant: TokenStream) -> TokenStream
    {
        let pairs: (Vec<TokenStream>, Vec<syn::Expr>);
        pairs = self.pairs.iter().map(|(key, value)| {
            let key = self.variant_name(key);
            let value: syn::Expr = syn::parse_str(&value).unwrap();

            (key, value)
        }).unzip();
        let (key, value) = pairs;

        quote!(
            match #variant {
                #(#key => #value, )*
            }
        )
    }
}

impl std::hash::Hash for Enum {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let scope = format!("{:?}", self.scope);

        self.id.hash(state);
        scope.hash(state);
    }
}

impl PartialEq for Enum {
    fn eq(&self, other: &Self) -> bool {
        let self_scope = format!("{:?}", self.scope);
        let other_scope = format!("{:?}", other.scope);

        self.id == other.id && self_scope == other_scope
    }
}

impl Eq for Enum {}
