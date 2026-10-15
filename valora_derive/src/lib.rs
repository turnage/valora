extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, *};

/// Derives `UniformSet` for a struct with named fields, laying the fields out
/// in declaration order as a WGSL uniform struct with the same field order.
#[proc_macro_derive(UniformSet)]
pub fn derive_uniform_set(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(fields),
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

    impl_uniform_set(input.ident, input.generics, fields)
}

fn impl_uniform_set(
    ident: Ident,
    generics: Generics,
    fields: FieldsNamed,
) -> proc_macro::TokenStream {
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let mut field_writes = TokenStream::new();
    field_writes.extend(fields.named.into_iter().map(|field| {
        let ident = field.ident.expect("Unwrapping name of named struct field");
        quote!(writer.write(&self.#ident);)
    }));

    quote!(
        impl #impl_generics UniformSet for #ident #type_generics #where_clause {
            fn write_uniforms(&self, writer: &mut UniformWriter) {
                #field_writes
            }
        }
    )
    .into()
}
