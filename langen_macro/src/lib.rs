use std::rc::Rc;

use parser::{Lr1Automaton, MetaSymbol, Rule, Symbol, Terminal};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use regex_automata::dfa::dense;
use syn::{
    parse::Parse, spanned::Spanned, token::Comma, Data, DeriveInput, Expr, ExprClosure, Fields,
    Ident, LitStr,
};

mod parser;

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

        let mut rules = vec![];
        let mut non_terminals = vec![];
        let mut terminals = vec![];
        let mut out_variant = None;
        let mut out_type = None;

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
                rules.push((variant.ident.clone(), input.parts, input.fun));
            }

            if !has_rule {
                terminals.push((
                    variant.ident.clone(),
                    matches!(variant.fields, Fields::Unnamed(_)),
                ));
            } else {
                let Fields::Unnamed(fields) = variant.fields else {
                    panic!("Every variant of grammar must have one unnamed field");
                };
                non_terminals.push(variant.ident.clone());
                if out_type.is_none() {
                    out_variant = Some(variant.ident.clone());
                    out_type = Some(fields.unnamed[0].ty.clone());
                }
            }
        }

        let parser_rules = rules
            .iter()
            .map(|rule| {
                Rc::new(Rule {
                    parts: rule
                        .1
                        .iter()
                        .map(|ident| {
                            if let Some(i) = non_terminals.iter().position(|e| e == ident) {
                                Symbol::NonTerminal(MetaSymbol::Normal(i))
                            } else if let Some(i) = terminals.iter().position(|(e, _)| e == ident) {
                                Symbol::Terminal(Terminal::Normal(i))
                            } else {
                                panic!("Symbol \"{ident}\" unknown (in rule for \"{}\")", rule.0);
                            }
                        })
                        .collect(),
                    result: MetaSymbol::Normal(
                        non_terminals
                            .iter()
                            .position(|e| *e == rule.0)
                            .expect("This comes from the variants, so should always exist"),
                    ),
                })
            })
            .collect();

        let mut automaton =
            Lr1Automaton::create(parser_rules, terminals.len(), non_terminals.len());
        automaton.build_automaton();
        automaton = automaton.to_lalr1();
        let (action, jump) = automaton.generate_tables();

        let mut action_code = vec![];

        for (i, action_row) in action.iter().enumerate() {
            for (symbol, action) in action_row {
                let code_symbol = match symbol {
                    Terminal::Normal(symbol_i) => {
                        let (ident, has_value) = &terminals[*symbol_i];
                        if *has_value {
                            quote! {Some((Self::#ident(v), span))}
                        } else {
                            quote! {Some((Self::#ident, span))}
                        }
                    }
                    Terminal::Eof => quote! {None},
                };
                let action = match action {
                    parser::Action::Shift(n) => match symbol {
                        Terminal::Normal(symbol_i) => {
                            let (ident, has_value) = &terminals[*symbol_i];
                            if *has_value {
                                quote! {
                                    stack.push(#n);
                                    symbol_stack.push((Self::#ident(v), span));
                                }
                            } else {
                                quote! {
                                    stack.push(#n);
                                    symbol_stack.push((Self::#ident, span));
                                }
                            }
                        }
                        Terminal::Eof => {
                            panic!("Can't shift in EOF");
                        }
                    },
                    parser::Action::Reduce(m) => {
                        let put_back = match symbol {
                            Terminal::Normal(symbol_i) => {
                                let (ident, has_value) = &terminals[*symbol_i];
                                if *has_value {
                                    quote! {input.push((Self::#ident(v), span));}
                                } else {
                                    quote! {input.push((Self::#ident, span));}
                                }
                            }
                            Terminal::Eof => quote! {},
                        };

                        let mut pop_code = vec![];
                        let mut fields = vec![];
                        let mut spans = vec![];
                        for (i, ident) in rules[*m].1.iter().enumerate() {
                            let var_ident = format_ident!("v{i}");
                            let span_ident = format_ident!("s{i}");

                            if non_terminals.contains(ident) {
                                pop_code.push(quote! {
                                            stack.pop();
                                            let Some((Self::#ident(#var_ident), #span_ident)) = symbol_stack.pop() else {unreachable!("Stack corrupted! (1)")};
                                        });
                                fields.push(var_ident);
                            } else {
                                if terminals
                                    .iter()
                                    .find_map(|(var_ident, has_value)| {
                                        if var_ident == ident {
                                            Some(*has_value)
                                        } else {
                                            None
                                        }
                                    })
                                    .expect("Has to be in terminals")
                                {
                                    pop_code.push(quote! {
                                                stack.pop();
                                                let Some((Self::#ident(#var_ident), #span_ident)) = symbol_stack.pop() else {unreachable!("Stack corrupted! (2)")};
                                            });
                                    fields.push(var_ident);
                                } else {
                                    pop_code.push(quote! {
                                                stack.pop();
                                                let Some((Self::#ident, #span_ident)) = symbol_stack.pop() else {unreachable!("Stack corrupted! (3)")};
                                            });
                                }
                            }
                            spans.push(span_ident);
                        }
                        pop_code = pop_code.into_iter().rev().collect();

                        let closure = &rules[*m].2;

                        let func_code = if spans.is_empty() {
                            quote! {
                                // TODO: maybe try to find actual values for this
                                let span = langen::Span { start: 0, end: 0 };
                                let r = (#closure)(span.clone());
                                let value = match r {
                                    Ok(v) => v,
                                    Err(e) => {
                                        return Err(langen::ParserError::ProcessError(num_tokens-input.len(), Box::new(e), span));
                                    }
                                };
                            }
                        } else {
                            let first = spans.first().expect("Can't be empty");
                            let last = spans.last().expect("Can't be empty");
                            quote! {
                                let span = langen::Span { start: #first.start, end: #last.end };
                                let r = (#closure)(span.clone(), #( #fields ),*);
                                let value = match r {
                                    Ok(v) => v,
                                    Err(e) => {
                                        return Err(langen::ParserError::ProcessError(num_tokens-input.len(), Box::new(e), span));
                                    }
                                };
                            }
                        };

                        let result = &rules[*m].0;
                        let mut jump_code = vec![];
                        let meta_i = non_terminals.iter().position(|elem| elem == result).expect("Must contain ident");

                        for (state, new_state) in &jump[meta_i] {
                            jump_code.push(quote!{
                                Some(#state) => {stack.push(#new_state)}
                            });
                        }

                        quote! {
                            #put_back
                            #( # pop_code )*
                            #func_code
                            symbol_stack.push((Self::#result(value), span));
                            match stack.last() {
                                #( #jump_code )*
                                _ => unreachable!("Stack corrupted! (4)"),
                            }
                        }
                    }
                    parser::Action::Accept => {
                        quote! {
                            let (Self::#out_variant(v), _) = symbol_stack.pop().expect("Stack corrupted! (5)") else {
                                unreachable!("Stack corrupted! (6)")
                            };
                            return Ok(v);
                        }
                    }
                };
                action_code.push(quote! {(Some(#i), #code_symbol) => {#action}});
            }
        }

        quote! {
            impl langen::Grammar for #name {
                type OUT = #out_type;

                fn parse(tokens: Vec<(Self, langen::Span)>) -> Result<Self::OUT, langen::ParserError<Self>> {
                    let num_tokens = tokens.len();
                    let mut input = tokens.into_iter().rev().collect::<Vec<_>>();
                    let mut symbol_stack: Vec<(Self, langen::Span)> = vec![];
                    let mut stack: Vec<usize> = vec![0];

                    loop {
                        // println!("{:?}\n\n{:?}\n\n{:?}\n\n\n", input, symbol_stack, stack);
                        match (stack.last(), input.pop()) {
                            #( #action_code )*
                            (None, _) => {return Err(langen::ParserError::UnexpectedEnd)} // This is something else, might even be unreachable, but i don't care
                            (_, None) => {return Err(langen::ParserError::UnexpectedEnd)}
                            (_, Some((token, span))) => {return Err(langen::ParserError::InvalidToken(num_tokens-input.len(), token, span))}
                        }
                    }
                }
            }
        }
        .into()
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
