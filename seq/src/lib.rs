use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse::ParseStream;

struct SeqParser {
    variable_ident: syn::Ident,
    start: isize,
    end: isize,
    body: proc_macro2::TokenStream,
}

impl syn::parse::Parse for SeqParser {
    // 解析形如 `N in 0..32 {......}` 这样的代码片段
    // 假定 `ParseStream` 当前右边对应的是一个可以解析为 `Ident` 类型的Token，
    // 如果真的是 `Ident` 类型节点，则返回Ok并将当前读取游标向后移动一个Token
    // 如果不是 `Ident` 类型节点，则返回Err，说明语法错误，直接返回。
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // 解析出 N
        let variable_ident: syn::Ident = input.parse()?;
        // 跳过`in`
        input.parse::<syn::Token!(in)>()?;
        // 解析出`0`数字
        let start: syn::LitInt = input.parse()?;
        // 跳过`..`
        input.parse::<syn::Token!(..)>()?;
        // 解析出`32`数字
        let end: syn::LitInt = input.parse()?;

        // 读取整个大括号里的内容，解析`{......}`
        let body_buf;
        syn::braced!(body_buf in input);
        let body: proc_macro2::TokenStream = body_buf.parse()?;

        return Ok(SeqParser{
            variable_ident,
            start: start.base10_parse()?,
            end: end.base10_parse()?,
            body,
        })
    }
}

impl SeqParser {
    fn expand(&self, token_stream: &proc_macro2::TokenStream, n: isize) -> proc_macro2::TokenStream {
        let buf = token_stream.clone().into_iter().collect::<Vec<proc_macro2::TokenTree>>();
        let mut ret = proc_macro2::TokenStream::new();
        for idx in 0..buf.len() {
            let tree_node = &buf[idx];
            match tree_node {
                proc_macro2::TokenTree::Group(g) => {
                    let new_stream = self.expand(&g.stream(), n);
                    // 加入括号
                    let wrap_in_group = proc_macro2::Group::new(g.delimiter(), new_stream);
                    ret.extend(quote::quote!(#wrap_in_group));
                }
                proc_macro2::TokenTree::Ident(i) => {
                    // 找到了指定的 ident
                    if i == &self.variable_ident {
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ret.extend(quote::quote!(#new_ident));
                    } else {
                        ret.extend(quote::quote!(#tree_node));
                    }
                }
                _ => {
                    // 对于标点符号和字面量原封不动的保存
                    ret.extend(quote::quote!(#tree_node));
                }
            }
        }
        ret
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq_parser = syn::parse_macro_input!(input as SeqParser);

    let mut ret = proc_macro2::TokenStream::new();
    for i in seq_parser.start..seq_parser.end {
        let ts = seq_parser.expand(&seq_parser.body, i);
        eprintln!("{}", ts);
        ret.extend(ts);
    }

    return ret.into();
}
