use proc_macro::TokenStream;
use std::ops::Deref;
use quote::{ToTokens};
use syn::Expr;
use syn::spanned::Spanned;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(ret) => {
            ret.into()
        }
        Err(e) => {
            e.to_compile_error().into()
        }
    }
}

// 第二关
fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let struct_name_ident = st.ident.clone();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.ident.span().clone());

    // 新结构体字段
    let builder_fields = generate_struct_fields(st)?;

    // 构造函数初始值
    let builder_fields_value = generate_struct_value(st)?;

    // 构造setter
    let builder_fields_setter = generate_struct_setter(st)?;

    // 构造build方法
    let builder_fn = generate_struct_build_fn(st)?;

    let ret = quote::quote!(
        pub struct #builder_name_ident {
            #builder_fields
        }

        impl #struct_name_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #(#builder_fields_value),*
                }
            }
        }

        impl #builder_name_ident {
            #builder_fields_setter

            #builder_fn
        }
    );
    Ok(ret)
}
type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;
fn get_fields_from_derive_input(st: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct{fields:syn::Fields::Named(syn::FieldsNamed{named, .. }), .. }) = &st.data {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(st, "Must Define On Struct"))
}
fn generate_struct_fields(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields  = get_fields_from_derive_input(st)?;
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: syn::Result<Vec<_>> = fields.iter().map(|f| {
        if let Some(inner_type) = get_generic_inner_type(&f.ty, "Option") {
            Ok(quote::quote!(
                std::option::Option<#inner_type>
            ))
        } else if get_field_user_specified_attrs(f)?.is_some() {
            let origin_type = &f.ty;
            Ok(quote::quote!(
                #origin_type
            ))
        } else {
            let original_type = &f.ty;
            Ok(quote::quote!(
                std::option::Option<#original_type>
            ))
        }
    }).collect();

    let types = types?;
    let ts = quote::quote!(
        #(#idents: #types),*
    );
    Ok(ts)
}
fn generate_struct_value(st: &syn::DeriveInput) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let fields  = get_fields_from_derive_input(st)?;
    let init_value: syn::Result<Vec<_>> = fields.iter().map(|f| {
        let ident = &f.ident;
        if get_field_user_specified_attrs(f)?.is_some() {
            Ok(quote::quote!(
                #ident: std::vec::Vec::new()
            ))
        } else {
            Ok(quote::quote!(
                #ident: std::option::Option::None
            ))
        }
    }).collect();


    Ok(init_value?)
}

fn generate_struct_setter(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields  = get_fields_from_derive_input(st)?;
    let idents: Vec<_> = fields.iter().map(|f|&f.ident).collect();
    let tys: Vec<_> = fields.iter().map(|f|&f.ty).collect();

    let mut final_token_stream = proc_macro2::TokenStream::new();

    for (idx, (ident, ty)) in idents.iter().zip(tys.iter()).enumerate() {
        let new_token_stream = if let Some(inner_type) = get_generic_inner_type(
            ty, "Option") {
            quote::quote!(
                fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        } else if let Some(ref user_specified_ident) = get_field_user_specified_attrs(&fields[idx])? {
            let inner_type = get_generic_inner_type(ty, "Vec").ok_or(
                syn::Error::new(fields[idx].span(), "'each' field must be a Vec type"))?;

            let mut ts_piece = quote::quote!(
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_type) -> &mut Self {
                    self.#ident.push(#user_specified_ident);
                    self
                }
            );

            if user_specified_ident != ident.as_ref().unwrap() {
                ts_piece.extend(quote::quote!(
                    fn #ident(&mut self, #ident:#ty) -> &mut Self {
                        self.#ident = #ident.clone();
                        self
                    }
                ));
            }
            ts_piece
        } else {
            quote::quote!(
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        };
        final_token_stream.extend(new_token_stream);
    }
    Ok(final_token_stream)
}

fn generate_struct_build_fn(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let tys: Vec<_> = fields.iter().map(|f|&f.ty).collect();

    let mut check_fields_pieces = Vec::new();
    for idx in 0..fields.len() {
        let ident = &fields[idx].ident.clone().unwrap();
        if get_generic_inner_type(tys[idx], "Option").is_none() &&
            get_generic_inner_type(tys[idx], "Vec").is_none() {
            check_fields_pieces.push(
                quote::quote!(
                    if self.#ident.is_none() {
                        let err = format!("{} field is missing", stringify!(#ident));
                        return std::result::Result::Err(err.into());
                    }
                )
            );
        }
    }

    let mut fill_fields = Vec::new();
    for idx in 0..fields.len() {
        let ident = &fields[idx].ident.clone().unwrap();
        if get_field_user_specified_attrs(&fields[idx])?.is_some() {
            fill_fields.push(
                quote::quote!(
                    #ident: self.#ident.clone()
                )
            );
        } else if get_generic_inner_type(tys[idx], "Option").is_none() {
            fill_fields.push(
                quote::quote!(
                    #ident: self.#ident.clone().unwrap()
                )
            );
        } else {
            fill_fields.push(
                quote::quote!(
                    #ident: self.#ident.clone()
                )
            );
        }
    }

    let struct_name = &st.ident;

    let token_stream = quote::quote!(
        pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
            #(#check_fields_pieces)*

            let ret = #struct_name{
                #(#fill_fields),*
            };

            std::result::Result::Ok(ret)
        }
    );
    Ok(token_stream)
}

#[allow(dead_code)]
fn get_optional_inner_type(t: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) = t {
        if let Some(last_seg) = segments.last() {
            if last_seg.ident.to_string() == "Option".to_string() {
                if let syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments{
                        args, .. }) = &last_seg.arguments {
                    if let Some(syn::GenericArgument::Type(t)) = args.last() {
                        return Some(t);
                    }
                }
            }
        }
    }

    None
}

fn get_field_user_specified_attrs(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            let v: Expr;
            if let Ok(r) = attr.parse_args() {
                v = r;
            } else {
                return Ok(None);
            }
            if let syn::Expr::Assign(syn::ExprAssign{left, right, ..}) = &v {
                if left.into_token_stream().to_string() == "each" {
                    if let Expr::Lit(syn::ExprLit{lit: syn::Lit::Str(l), ..}) = right.deref() {
                        return Ok(Some(syn::Ident::new(
                            l.value().as_ref(),
                            attr.span())));
                    }
                } else {
                    return Err(syn::Error::new_spanned(attr.meta.to_token_stream(),
                                                       r#"expected `builder(each = "...")`"#));
                }
            }
        } else {
            return Ok(None);
        }
    }
    return Ok(None);
}

fn get_generic_inner_type<'a>(t: &'a syn::Type, outer_ident_name: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) = t {
        if let Some(last_seg) = segments.last() {
            if last_seg.ident.to_string() == outer_ident_name.to_string() {
                if let syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments{
                        args, .. }) = &last_seg.arguments {
                    if let Some(syn::GenericArgument::Type(t)) = args.last() {
                        return Some(t);
                    }
                }
            }
        }
    }

    None
}