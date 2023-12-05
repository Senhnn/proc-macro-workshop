use proc_macro::TokenStream;
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
        let mut idx = 0;
        while idx < buf.len() {
            let tree_node = &buf[idx];
            match tree_node {
                proc_macro2::TokenTree::Group(g) => {
                    let new_stream = self.expand(&g.stream(), n);
                    // 加入括号
                    let wrap_in_group = proc_macro2::Group::new(g.delimiter(), new_stream);
                    ret.extend(quote::quote!(#wrap_in_group));
                }
                proc_macro2::TokenTree::Ident(prefix) => {
                    if idx + 2 < buf.len() { // 向后预读两个元素，判断这两个元素是不是'~'和'N'
                        if let proc_macro2::TokenTree::Punct(punct) = &buf[idx+1] {
                            if punct.as_char() == '~' {
                                if let proc_macro2::TokenTree::Ident(n_ident) = &buf[idx+2] {
                                    if n_ident == &self.variable_ident {
                                        if prefix.span().end() == punct.span().start()
                                            && punct.span().end() == n_ident.span().start() { // 这三个元素是否连续
                                            let new_ident_literal = format!("{}{}", prefix.to_string(), n.to_string());
                                            let new_ident = proc_macro2::Ident::new(new_ident_literal.as_str(), prefix.span());
                                            ret.extend(quote::quote!(#new_ident));
                                            idx += 3;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if prefix == &self.variable_ident {
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ret.extend(quote::quote!(#new_ident));
                        idx += 1;
                        continue;
                    }
                    ret.extend(quote::quote!(#tree_node));
                }
                _ => {
                    // 对于标点符号和字面量原封不动的保存
                    ret.extend(quote::quote!(#tree_node));
                }
            }
            idx += 1;
        }
        ret
    }

    fn find_block_to_expand_and_do_expand(&self, c: syn::buffer::Cursor) -> (proc_macro2::TokenStream, bool) {
        let mut found = false;
        let mut ret = proc_macro2::TokenStream::new();

        let mut cursor = c;
        while !cursor.eof() {
            if let Some((punct_prefix, cursor_1)) = cursor.punct() {
                if punct_prefix.as_char() == '#' {
                    if let Some((group_cur, _, cursor_2)) = cursor_1.group(proc_macro2::Delimiter::Parenthesis) {
                        if let Some((punct_suffix, cursor_3)) = cursor_2.punct() {
                            if punct_suffix.as_char() == '*' {
                                for i in self.start..self.end {
                                    let t = self.expand(&group_cur.token_stream(), i);
                                    ret.extend(t);
                                }
                                cursor = cursor_3;
                                found = true;
                                continue;
                            }
                        }
                    }
                }
            }
            if let Some((group_cur, _ , next_cur)) = cursor.group(proc_macro2::Delimiter::Brace) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!({#t}));
                cursor = next_cur;
                continue;
            } else if let Some((group_cur, _ , next_cur)) = cursor.group(proc_macro2::Delimiter::Bracket) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!([#t]));
                cursor = next_cur;
                continue
            } else if let Some((group_cur,_, next_cur)) = cursor.group(proc_macro2::Delimiter::Parenthesis) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!((#t)));
                cursor = next_cur;
                continue
            }else if let Some((punct ,next_cur)) = cursor.punct() {
                ret.extend(quote::quote!(#punct));
                cursor = next_cur;
                continue
            } else if let Some((ident ,next_cur)) = cursor.ident() {
                ret.extend(quote::quote!(#ident));
                cursor = next_cur;
                continue
            } else if let Some((literal ,next_cur)) = cursor.literal() {
                ret.extend(quote::quote!(#literal));
                cursor = next_cur;
                continue
            } else if let Some((lifetime ,next_cur)) = cursor.lifetime() {
                // lifetime这种特殊的分类也是用cursor模式来处理的时候特有的，之前`proc_macro2::TokenTree`里面没有定义这个分类
                ret.extend(quote::quote!(#lifetime));
                cursor = next_cur;
                continue
            }
        }
        (ret, found)
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq_parser = syn::parse_macro_input!(input as SeqParser);

    let mut ret = proc_macro2::TokenStream::new();

    let buffer = syn::buffer::TokenBuffer::new2(seq_parser.body.clone());

    let(ret_1, expanded) =  seq_parser.find_block_to_expand_and_do_expand(buffer.begin());
    if expanded {
        return ret_1.into();
    }

    for i in seq_parser.start..seq_parser.end {
        let ts = seq_parser.expand(&seq_parser.body, i);
        ret.extend(ts);
    }

    return ret.into();
}
