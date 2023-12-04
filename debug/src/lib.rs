use proc_macro::TokenStream;
use quote::ToTokens;
use std::collections::HashMap;
use syn::parse_quote;
use syn::spanned::Spanned;
use syn::visit::{self, Visit};

struct TypePathVisitor {
    generic_type_names: Vec<String>, // 记录所有泛型参数的名字
    associated_types: HashMap<String, Vec<syn::TypePath>>, // 记录所有满足条件的语法树节点
}

// 遍历语法树的节点
impl<'ast> Visit<'ast> for TypePathVisitor {
    fn visit_type_path(&mut self, node: &syn::TypePath) {
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                self.associated_types
                    .entry(generic_type_name)
                    .or_insert(Vec::new())
                    .push(node.clone());
            }
        }

        visit::visit_type_path(self, node);
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        syn::Result::Ok(ret) => ret.into(),
        syn::Result::Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let origin_struct_ident = &st.ident;

    let mut generic_data = st.generics.clone();

    if let Some(hatch) = get_struct_escape_hatch(st) {
        generic_data.make_where_clause();
        generic_data
            .where_clause
            .as_mut()
            .unwrap()
            .predicates
            .push(syn::parse_str(hatch.as_str()).unwrap());
    } else {
        // 构建两个列表，一个是PhantomData中使用到的泛型参数，另一个是输入结构体中所有的参数。
        let fields = get_struct_all_fields(st)?;
        let mut field_type_names = Vec::new();
        let mut phantom_data_type_param_names = Vec::new();
        for field in fields.iter() {
            if let Some(s) = get_field_type_name(field)? {
                field_type_names.push(s);
            }
            if let Some(s) = get_phantom_data_generic_type_name(field)? {
                phantom_data_type_param_names.push(s);
            }
        }
        let associated_types_map = get_generic_associated_types(st);
        for g in generic_data.params.iter_mut() {
            if let syn::GenericParam::Type(t) = g {
                let type_param_name = t.ident.to_string();

                if phantom_data_type_param_names.contains(&type_param_name)
                    && !field_type_names.contains(&type_param_name)
                {
                    continue;
                }

                if associated_types_map.contains_key(&type_param_name)
                    && !field_type_names.contains(&type_param_name)
                {
                    continue;
                }

                t.bounds.push(syn::parse_quote!(std::fmt::Debug));
            }
        }

        generic_data.make_where_clause();
        for (_, associated_types) in associated_types_map {
            for associated_type in associated_types {
                generic_data
                    .where_clause
                    .as_mut()
                    .unwrap()
                    .predicates
                    .push(parse_quote!(#associated_type:std::fmt::Debug));
            }
        }
    }

    let (impl_generics, type_generics, where_clause) = generic_data.split_for_impl();

    let core_token_stream = generate_fmt_fn_core(st)?;

    let final_token_stream = quote::quote!(
        impl #impl_generics std::fmt::Debug for #origin_struct_ident #type_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                #core_token_stream
            }
        }
    );
    // eprintln!("{}", quote::quote!(#final_token_stream));
    syn::Result::Ok(final_token_stream)
}

fn generate_fmt_fn_core(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let origin_struct_ident = &st.ident;
    let origin_struct_ident_literal = origin_struct_ident.to_string();
    let mut res_token_stream = quote::quote!(
        f.debug_struct(#origin_struct_ident_literal)
    );

    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = &st.data {
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

fn get_struct_all_fields(st: &syn::DeriveInput) -> syn::Result<&syn::Fields> {
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = &st.data {
        return syn::Result::Ok(fields);
    }
    return syn::Result::Err(syn::Error::new(st.span(), "should be a struct"));
}

fn get_custom_format_of_field(field: &syn::Field) -> syn::Result<Option<String>> {
    for attr in &field.attrs {
        if let syn::Meta::NameValue(syn::MetaNameValue { path, value, .. }) = &attr.meta {
            if path.to_token_stream().to_string() == "debug" {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(l),
                    ..
                }) = value
                {
                    return Ok(Some(l.value()));
                }
            } else {
                return Err(syn::Error::new_spanned(
                    attr.meta.to_token_stream(),
                    r#"expected `#[debug = "..."]`"#,
                ));
            }
        } else {
            return Ok(None);
        }
    }

    Ok(None)
}

fn get_phantom_data_generic_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = &field.ty
    {
        if let Some(syn::PathSegment { ident, arguments }) = segments.last() {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = arguments
                {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(type_path))) =
                        args.first()
                    {
                        if let Some(generic_ident) = type_path.path.segments.first() {
                            return Ok(Some(generic_ident.ident.to_string()));
                        }
                    }
                }
            }
        }
    }

    Ok(None)
}

fn get_field_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = &field.ty
    {
        if let Some(syn::PathSegment { ident, .. }) = segments.last() {
            return Ok(Some(ident.to_string()));
        }
    }
    Ok(None)
}

fn get_generic_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    let origin_generic_param_names: Vec<String> = st
        .generics
        .params
        .iter()
        .filter_map(|f| {
            if let syn::GenericParam::Type(ty) = f {
                return Some(ty.ident.to_string());
            }
            return None;
        })
        .collect();

    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,
        associated_types: HashMap::new(),
    };

    visitor.visit_derive_input(st);
    return visitor.associated_types;
}

fn get_struct_escape_hatch(st: &syn::DeriveInput) -> Option<String> {
    if let Some(inert_attr) = st.attrs.first() {
        if inert_attr
            .meta
            .path()
            .segments
            .first()
            .unwrap()
            .ident
            .to_string()
            == "debug"
        {
            if let Ok(syn::Expr::Assign(syn::ExprAssign { left, right, .. })) =
                inert_attr.parse_args::<syn::Expr>()
            {
                if let syn::Expr::Path(syn::ExprPath {
                    path: syn::Path { segments, .. },
                    ..
                }) = *left
                {
                    if segments[0].ident.to_string() == "bound" {
                        if let syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(str),
                            ..
                        }) = *right
                        {
                            return Some(str.value());
                        }
                    }
                }
            }
        }
    }
    None
}
