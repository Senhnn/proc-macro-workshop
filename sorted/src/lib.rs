use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{ExprMatch};
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input_item = syn::parse_macro_input!(input as syn::Item);

    match do_expand(&input_item) {
        Ok(token_stream) => token_stream.into(),
        Err(err) => {
            let mut e = err.to_compile_error();
            e.extend(input_item.to_token_stream());
            e.into()
        },
    }
}

fn do_expand(item: &syn::Item) -> syn::Result<proc_macro2::TokenStream> {
    match item {
        syn::Item::Enum(e) => {
            check_enum_order(e)
        }
        _ => syn::Result::Err(syn::Error::new(proc_macro2::Span::call_site(), "expected enum or match expression"))
    }
}

fn check_enum_order(item_enum: &syn::ItemEnum) -> syn::Result<proc_macro2::TokenStream> {
    let origin_order = item_enum.variants.iter().map(|v| (v.ident.to_string(), v)).
        collect::<Vec<(String, &syn::Variant)>>();

    let mut sort_order = origin_order.clone();
    sort_order.sort_by(|a, b| {
        a.0.cmp(&b.0)
    });

    for (a, b) in origin_order.iter().zip(sort_order) {
        if a.0 != b.0 {
            return syn::Result::Err(syn::Error::new_spanned(
                &b.1.ident, format!("{} should sort before {}", b.0, a.0)));
        }
    }

    return syn::Result::Ok(item_enum.to_token_stream());
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    // check会作用于一个函数上
    let mut input_item = syn::parse_macro_input!(input as syn::ItemFn);

    match do_match_expand(&mut input_item) {
        Ok(token_stream) => token_stream.into(),
        Err(err) => {
            let mut e = err.to_compile_error();
            e.extend(input_item.to_token_stream());
            e.into()
        },
    }
}

fn do_match_expand(item_fn: &mut syn::ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let mut visitor = MatchVisitor{err:None};
    visitor.visit_item_fn_mut(item_fn);
    match &visitor.err {
        None => {
            syn::Result::Ok(item_fn.to_token_stream())
        },
        Some(e) => {
            syn::Result::Err(e.clone().into())
        }
    }
}

struct MatchVisitor {
    err: Option<syn::Error>,
}

impl syn::visit_mut::VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, expr_match: &mut ExprMatch) {
        let mut target_index = None;
        for (idx, attr) in expr_match.attrs.iter().enumerate() {
            if let syn::Meta::Path(path) = &attr.meta {
                if trans_path_to_string(path) == "sorted" {
                    target_index = Some(idx);
                    break;
                }
            }
        }

        match target_index {
            None => {
                syn::visit_mut::visit_expr_match_mut(self, expr_match);
                return;
            },
            Some(idx) => {
                expr_match.attrs.remove(idx);

                let mut match_arm_names: Vec<(String, &dyn ToTokens)> = Vec::new();

                for arm in &expr_match.arms {
                    match &arm.pat {
                        syn::Pat::Path(p) => {
                            match_arm_names.push((trans_path_to_string(&p.path), &p.path));
                        },
                        syn::Pat::TupleStruct(p)=>{
                            match_arm_names.push((trans_path_to_string(&p.path), &p.path));
                        },
                        syn::Pat::Struct(p)=>{
                            match_arm_names.push((trans_path_to_string(&p.path), &p.path));
                        },
                        syn::Pat::Ident(p) => {
                            match_arm_names.push((p.ident.to_string(), &p.ident));
                        },
                        syn::Pat::Wild(p) => {
                            match_arm_names.push(("_".to_string(), &p.underscore_token));
                        },
                        _ => {
                            self.err = Some(syn::Error::new(arm.pat.span(), "unsupported by #[sorted]"));
                            return;
                        }
                    }
                }
                let mut sorted_arm_names = match_arm_names.clone();
                sorted_arm_names.sort_by(|a, b| {
                    a.0.cmp(&b.0)
                });
                for (a, b) in match_arm_names.iter().zip(sorted_arm_names.iter()) {
                    if a.0 != b.0 {
                        self.err = Some(syn::Error::new_spanned(b.1, format!("{} should sort before {}", b.0, a.0)));
                        return;
                    }
                }
                syn::visit_mut::visit_expr_match_mut(self, expr_match);
                return;
            }
        }
    }
}

fn trans_path_to_string(path: &syn::Path) -> String {
    let mut buf = Vec::new();
    if let Some(_) = &path.leading_colon {
        buf.push("::".to_string());
    }
    for s in &path.segments {
        buf.push(s.ident.to_string());
    }
    buf.join("::")
}