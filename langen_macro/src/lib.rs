use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use regex_automata::dfa::dense;
use syn::{
    parse::Parse, spanned::Spanned, token::Comma, Data, DeriveInput, Expr, ExprClosure, Fields, Ident, LitStr
};

#[proc_macro_derive(Tokens, attributes(ignored, token))]
pub fn tokens_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    if let Data::Enum(data) = input.data {
        let name = input.ident;

        let mut token_indices = vec![];
        let mut token_code = vec![];
        let mut token_patterns = vec![];

        for re in input.attrs.iter().filter_map(|attr| {
            if attr.path().is_ident("ignored") {
                let t: LitStr = attr.parse_args().unwrap_or_else(|_| {
                    panic!("ignored argument for \"{}\" must be string literal", name)
                });
                Some(t)
            } else {
                None
            }
        }) {
            token_indices.push(token_code.len());
            token_code.push(quote! {continue;});
            token_patterns.push(re.value());
        }

        for variant in data.variants {
            for input in variant.attrs.iter().filter_map(|attr| {
                if attr.path().is_ident("token") {
                    let t: TokenInput = attr.parse_args().unwrap_or_else(|e| {
                        panic!(
                            "Invalid arguments for token argument for \"{}\": {e}",
                            variant.ident
                        )
                    });
                    Some(t)
                } else {
                    None
                }
            }) {
                let ident = &variant.ident;

                token_indices.push(token_code.len());

                token_code.push(match input.fun {
                    Some(closure) => {
                        if let Fields::Unnamed(_fields) = &variant.fields {
                            quote! {
                                let r = (#closure)(&input[start..end]);
                                match r {
                                    Ok(v) => Self::#ident(v),
                                    Err(e) => {
                                        return Err(langen::LexerError::ProcessError(Box::new(e), span))
                                    }
                                }
                            }
                        } else {
                            panic!(
                                "Variant \"{}\" must be one-length unnamed when having process function",
                                variant.ident
                            )
                        }
                    }
                    None => {
                        if let Fields::Unit = variant.fields {
                            quote! {Self::#ident}
                        } else {
                            panic!(
                                "Non-unit token variant \"{}\" must have process function",
                                variant.ident
                            )
                        }
                    }
                });
                token_patterns.push(input.re.value());
            }
        }

        let dfa = dense::DFA::new_many(&token_patterns).expect("Couldn't build regex automaton");
        let (bytes, pad) = dfa.to_bytes_little_endian();
        let le_dfa_bytes = &bytes[pad..];
        let (bytes, pad) = dfa.to_bytes_big_endian();
        let be_dfa_bytes = &bytes[pad..];

        quote! {
            impl langen::Tokens for #name {
                fn scan(input: &str) -> Result<Vec<(Self, langen::Span)>, langen::LexerError> {
                    const DFA: &langen::regex_automata::util::wire::AlignAs<[u8], u32> = &langen::regex_automata::util::wire::AlignAs {
                        _align: [],
                        #[cfg(target_endian = "big")]
                        bytes: [#(#be_dfa_bytes),*],
                        #[cfg(target_endian = "little")]
                        bytes: [#(#le_dfa_bytes),*],
                    };

                    // This is generated above, so we can always safely expect
                    let (dfa, _) = langen::regex_automata::dfa::dense::DFA::from_bytes(&DFA.bytes).expect("Couldn't deserialize dfa");

                    let mut re_input = langen::regex_automata::Input::new(input).anchored(langen::regex_automata::Anchored::Yes);
                    let mut tokens = vec![];
                    let mut current = 0;

                    while current != input.len() {
                        re_input.set_start(current);
                        // Input should always be fine
                        use langen::regex_automata::dfa::Automaton;
                        if let Some(m) = dfa.try_search_fwd(&re_input).expect("Regex Error") {
                            // println!("{} {:?}", current, m);
                            let start = current;
                            current = m.offset();
                            let end = current;

                            let span = langen::Span::new(start, end);
                            let token = match m.pattern().as_usize() {
                                #(#token_indices => {#token_code})*
                                _ => {unreachable!("Every pattern has to come from a regex")}
                            };

                            tokens.push((token, span));
                        } else {
                            return Err(langen::LexerError::NoToken(current));
                        }
                    }

                    Ok(tokens)
                }
            }
        }.into()
    } else {
        panic!("Langen can only be used on enum");
    }
}

struct TokenInput {
    re: LitStr,
    fun: Option<ExprClosure>,
}

impl Parse for TokenInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            re: input.parse()?,
            fun: match input.parse::<Comma>() {
                Ok(_) => {
                    let expr: Expr = input.parse()?;
                    if let Expr::Closure(closure) = expr {
                        Some(closure)
                    } else {
                        return Err(syn::Error::new(
                            expr.span(),
                            "Second argument to token must be closure",
                        ));
                    }
                }
                Err(_) => None,
            },
        })
    }
}

#[proc_macro_derive(Grammar, attributes(rule))]
pub fn grammar_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    if let Data::Enum(data) = input.data {
        let name = input.ident;

        let rules = vec![];
        let non_terminals = vec![];

        for variant in data.variants {
            let mut has_rule = false;
            for input in variant.attrs.iter().filter_map(|attr| {
                if attr.path().is_ident("rule") {
                    has_rule = true;
                    let t: RuleInput = attr.parse_args().unwrap_or_else(|e| {
                        panic!(
                            "Invalid arguments for rule argument for \"{}\": {e}",
                            variant.ident
                        )
                    });
                    Some(t)
                } else {
                    None
                }
            }) {

            }

            if !has_rule {
                non_terminals.push((variant.ident.clone(), matches!(variant.fields, Fields::Unnamed(_))));
            }
        }
                

        quote! {
            impl langen::Tokens for #name {
                fn scan(input: &str) -> Result<Vec<(Self, langen::Span)>, langen::LexerError> {
                    
                }
            }
        }.into()
    } else {
        panic!("Langen can only be used on enum");
    }
}

struct RuleInput {
    fun: ExprClosure,
    parts: Vec<Ident>,
}

impl Parse for RuleInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let expr: Expr = input.parse()?;
        let fun = if let Expr::Closure(closure) = expr {
            closure
        } else {
            return Err(syn::Error::new(
                expr.span(),
                "First argument to rule must be closure",
            ));
        };
        let mut parts = vec![];
        loop {
            if input.parse::<Comma>().is_err() {
                break;
            }
            parts.push(input.parse()?);
        }
        Ok(Self { fun, parts })
    }
}