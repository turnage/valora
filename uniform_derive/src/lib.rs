#![recursion_limit = "128"]
extern crate proc_macro;

use heck::CamelCase;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::convert::TryFrom;
use syn::{punctuated::*, spanned::Spanned, token::*, *};

type Result<T> = std::result::Result<T, Error>;

#[proc_macro_derive(UniformSet)]
pub fn validate_fidl_table(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let fields = if let Data::Struct(DataStruct {
        fields: syn::Fields::Named(fields),
        ..
    }) = input.data
    {
        fields
    } else {
        return Error::new(
            input.span(),
            "A uniform set may only be derived from a struct with named fields.",
        )
        .to_compile_error()
        .into();
    };

    impl_uniform_set(input.ident, fields)
}

fn impl_uniform_set(ident: Ident, fields: syn::FieldsNamed) -> proc_macro::TokenStream {
    let mut field_visits = TokenStream::new();

    field_visits.extend(fields.named.into_iter().map(|field| {
        let ident = field.ident.expect("Unwrapping name of named struct field");
        quote!(f(stringify!(#ident), self.#ident.into_uniform_value()))
    }));

    quote!(
        impl Uniforms for #ident {
            fn visit_values<'a, F: FnMut(&str, UniformValue<'a>)>(&'a self, mut f: F) {
                #field_visits
            }
        }
    )
    .into()
}
