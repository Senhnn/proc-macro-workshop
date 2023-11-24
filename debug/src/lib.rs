use proc_macro::TokenStream;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        syn::Result::Ok(ret) => {
            ret.into()
        }
        syn::Result::Err(e) => {
            e.to_compile_error().into()
        }
    }
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let origin_struct_ident = &st.ident;
    let origin_struct_ident_literal = origin_struct_ident.to_string();

    let struct_all_fields_ident = get_struct_all_fields_ident(st)?;
    let struct_all_fields_name: Vec<String> = struct_all_fields_ident.iter().map(|f| f.to_string()).collect();

    let final_token_stream = quote::quote!(
        impl std::fmt::Debug for #origin_struct_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                f.debug_struct(#origin_struct_ident_literal)
                #(.field(#struct_all_fields_name, &self.#struct_all_fields_ident))*
                .finish()
            }
        }
    );
    syn::Result::Ok(final_token_stream)
}

fn get_struct_all_fields_ident(st: &syn::DeriveInput) -> syn::Result<Vec<&syn::Ident>> {
    let mut fields_ident = Vec::new();
    if let syn::Data::Struct(syn::DataStruct{fields, ..}) = &st.data {
        for field in fields {
            if let Some(ident) = &field.ident {
                fields_ident.push(ident);
            }
        }
    }
    return syn::Result::Ok(fields_ident)
}