use proc_macro::TokenStream;
use quote::ToTokens;
use syn::Field;

#[proc_macro_derive(CustomDebug, attributes(debug))]
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
    // let origin_struct_ident_literal = origin_struct_ident.to_string();

    let mut generic_data = st.generics.clone();
    for g in generic_data.params.iter_mut() {
        if let syn::GenericParam::Type(t) = g {
            t.bounds.push(syn::parse_quote!(std::fmt::Debug));
        }
    }

    let (impl_generics, type_generics, where_clause) = generic_data.split_for_impl();
    // eprintln!("{:?}", impl_generics);
    // eprintln!("{:?}", type_generics);
    // eprintln!("{:?}", where_clause);

    let core_token_stream = generate_fmt_fn_core(st)?;

    let final_token_stream = quote::quote!(
        impl #impl_generics std::fmt::Debug for #origin_struct_ident #type_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                #core_token_stream
            }
        }
    );
    syn::Result::Ok(final_token_stream)
}

fn generate_fmt_fn_core(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let origin_struct_ident = &st.ident;
    let origin_struct_ident_literal = origin_struct_ident.to_string();
    let mut res_token_stream = quote::quote!(
        f.debug_struct(#origin_struct_ident_literal)
    );

    if let syn::Data::Struct(syn::DataStruct{fields, ..}) = &st.data {
        for field in fields {
            let mut format_str = "{:?}".to_string();
            if let Some(fs) = get_custom_format_of_field(field)? {
                format_str = fs;
            }
            if let Some(ident) = &field.ident {
                let field_name_literal = ident.to_string();
                res_token_stream.extend(quote::quote!(
                    .field(#field_name_literal, &format_args!(#format_str, self.#ident))));
            }
        }
    }
    res_token_stream.extend(quote::quote!(
        .finish()
    ));
    Ok(res_token_stream)
}

#[allow(dead_code)]
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

fn get_custom_format_of_field(field: &syn::Field) -> syn::Result<Option<String>> {
    for attr in &field.attrs {
        if let syn::Meta::NameValue(syn::MetaNameValue{path, value, .. }) = &attr.meta {
            if path.to_token_stream().to_string() == "debug" {
                if let syn::Expr::Lit(syn::ExprLit{lit: syn::Lit::Str(l), ..}) = value {
                    return Ok(Some(l.value()));
                }
            } else {
                return Err(syn::Error::new_spanned(attr.meta.to_token_stream(),
                                                   r#"expected `#[debug = "..."]`"#));
            }
        } else {
            return Ok(None);
        }
    }

    Ok(None)
}

// fn get_phantom_data_generic_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
//     if let syn::Type::Path(syn::TypePath{path: syn::Path{segments, ..}, ..}) = &field.ty {
//         if let Some(syn::PathSegment{ident, arguments}) = segments.last() {
//
//         }
//     }
//
//     Ok(None)
// }