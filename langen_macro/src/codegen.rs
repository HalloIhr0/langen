use std::collections::HashMap;

use crate::{finite_automaton::FiniteAutomaton, lexer::TokenVariant, parser::*};
use proc_macro2::TokenStream;
use quote::*;
use syn::{Fields, Ident};

pub fn generate_scan(
    dfa: FiniteAutomaton<(), Option<char>>,
    tokens: &[TokenVariant],
) -> TokenStream {
    let mut transitions = HashMap::new();
    let num_states = dfa.num_states;
    for transition in &dfa.transitions {
        transitions
            .entry(transition.transition.unwrap())
            .or_insert_with(|| vec![num_states; num_states]);
        transitions
            .get_mut(&transition.transition.unwrap())
            .unwrap()[transition.from_state] = transition.to_state;
    }

    let mut chars = Vec::new();
    let mut identifiers = Vec::new();
    let mut tables = Vec::new();
    for (c, t) in &transitions {
        chars.push(*c);
        identifiers.push(format_ident!("c{:X}", *c as u32));
        tables.push(quote! {#(#t),*});
    }
    let mut end_states_code = Vec::new();
    let mut end_states_ignored = Vec::new();
    for state in dfa.end_states {
        match state.token {
            Some(token) => {
                let span = token.span();
                let token = tokens.iter().find(|t| t.name == token).expect(
                    "The program shouldn't be able to just make these things up (hopefully)",
                );
                let end_state = state.state;
                let end_token = &token.name;
                end_states_code.push(match &token.fields {
                    syn::Fields::Named(fields) => {
                        let closure = token.ast_fun.as_ref().unwrap_or_else(|| panic!("{} has unnamed fields, but doesn't have a closure", token.name));
                        let mut field_names = vec![];
                        let mut names = vec![];
                        for (i, field) in fields.named.iter().enumerate() {
                            names.push(format_ident!("f{}", i));
                            field_names.push(field.ident.as_ref().expect("This should be named"))
                        }
                        quote_spanned!(span=>
                            #end_state => {
                                let (#(#names),*,) = ((#closure)(current_text)).map_err(|e: String| langen::errors::LexerError::AstError(e))?;
                                tokens.push(Self::#end_token{#(#field_names: #names),*});
                            }
                        )
                    },
                    syn::Fields::Unnamed(fields) => {
                        let closure = token.ast_fun.as_ref().unwrap_or_else(|| panic!("{} has unnamed fields, but doesn't have a closure", token.name));
                        let mut names = vec![];
                        for i in 0..fields.unnamed.len() {
                            names.push(format_ident!("f{}", i));
                        }
                        quote_spanned!(span=>
                            #end_state => {
                                let (#(#names),*,) = ((#closure)(current_text)).map_err(|e: String| langen::errors::LexerError::AstError(e))?;
                                tokens.push(Self::#end_token(#(#names),*));
                            }
                        )
                    },
                    syn::Fields::Unit => quote_spanned!(span=>
                        #end_state => tokens.push(Self::#end_token)
                    ),
                });
            }
            None => {
                end_states_ignored.push(state.state);
            }
        }
    }
    let start_state = dfa.start_state;
    quote! {
        pub fn scan(input: &str) -> Result<Vec<Self>, langen::errors::LexerError> {
            use std::error::Error;
            #( static #identifiers: [usize; #num_states]  = [#tables]; )*
            let mut tokens = Vec::new();
            let mut last = #num_states;
            let mut current = #start_state;
            let mut current_text = String::new();
            for (i, c) in input.chars().enumerate() {
                current = match c {
                    #( #chars => #identifiers[current], )*
                    _ => {return Err(langen::errors::LexerError::InvalidChar(i))}
                };
                if current == #num_states {
                    match last {
                        #( #end_states_code, )*
                        #( #end_states_ignored => {}, )*
                    _ => {return Err(langen::errors::LexerError::InvalidChar(i-1))}
                    };
                    current_text = String::new();
                    current = match c {
                        #( #chars => #identifiers[#start_state], )*
                        _ => {return Err(langen::errors::LexerError::InvalidChar(i))}
                    };
                }
                current_text.push(c);
                last = current;
            }
            match current {
                #( #end_states_code, )*
                #( #end_states_ignored => {}, )*
            _ => {return Err(langen::errors::LexerError::IncompleteToken)}
            };
            Ok(tokens)
        }
    }
}

pub fn generate_parse(
    grammar: &Grammar,
    table: &ParserTable,
    symbol_fields: &HashMap<Ident, Fields>,
) -> TokenStream {
    let start = &grammar.symbols[grammar.rules[0].0]
        .get_ident()
        .expect("Has to be a rule");
    let (return_type, return_code) = match &symbol_fields[start] {
        Fields::Named(fields) => {
            let types = fields.named.iter().map(|f| f.ty.clone());
            let mut field_names = vec![];
            let mut names = vec![];
            for (i, field) in fields.named.iter().enumerate() {
                names.push(format_ident!("f{}", i));
                field_names.push(field.ident.as_ref().expect("This should be named"))
            }
            (
                quote!((#(#types),*,)),
                quote!(
                    return Ok(if let Some(Self::#start{#(#field_names: #names),*}) = symbol_stack.pop() {
                        (#(#names),*,)
                    } else {
                        unreachable!()
                    });
                ),
            )
        }
        Fields::Unnamed(fields) => {
            let types = fields.unnamed.iter().map(|f| f.ty.clone());
            let mut names = vec![];
            for i in 0..fields.unnamed.len() {
                names.push(format_ident!("f{}", i));
            }
            (
                quote!((#(#types),*,)),
                quote!(
                    return Ok(if let Some(Self::#start(#(#names),*)) = symbol_stack.pop() {
                        (#(#names),*,)
                    } else {
                        unreachable!()
                    });
                ),
            )
        }
        Fields::Unit => (quote!(()), quote!(return Ok(());)),
    };

    let mut actions = vec![HashMap::new(); table.num_states];
    let mut eof_actions = HashMap::new();
    for k in table.action_table.keys() {
        match &k.1 {
            ParserSymbol::Symbol(ident) => actions[k.0].insert(
                ident,
                generate_parser_action(grammar, table, k, symbol_fields, &return_code),
            ),
            ParserSymbol::Terminal(ident) => actions[k.0].insert(
                ident,
                generate_parser_action(grammar, table, k, symbol_fields, &return_code),
            ),
            ParserSymbol::Eof => eof_actions.insert(
                k.0,
                generate_parser_action(grammar, table, k, symbol_fields, &return_code),
            ),
            ParserSymbol::Epsilon => panic!(),
            ParserSymbol::Start => panic!(),
        };
    }
    let mut actions_keys = vec![];
    let mut actions_values = vec![];
    for (i, action) in actions.iter().enumerate() {
        actions_keys.push(i);
        let mut idents = vec![];
        let mut codes = vec![];
        for (ident, code) in action {
            idents.push(match symbol_fields[*ident] {
                Fields::Named(_) => quote!(#ident{..}),
                Fields::Unnamed(_) => quote!(#ident(..)),
                Fields::Unit => quote!(#ident),
            });
            codes.push(code);
        }
        actions_values.push(quote! {
            match current {
                #( Self::#idents => {#codes}, )*
                _ => {return Err(langen::errors::ParserError::InvalidSymbol(i))}
            }
        });
    }
    let mut eof_keys = vec![];
    let mut eof_values = vec![];
    for (k, v) in eof_actions {
        eof_keys.push(k);
        eof_values.push(v);
    }

    quote! {
        pub fn parse(input: Vec<Self>) -> Result<#return_type, langen::errors::ParserError> {
            use std::error::Error;
            let mut state_stack = vec![0usize];
            let mut symbol_stack = vec![];
            let mut i = 0usize;
            let mut iter = input.iter();
            let mut eof = false;
            let mut current = match iter.next() {
                Some(x) => x,
                None => {return Err(langen::errors::ParserError::UnexpectedEnd);},
            };
            loop {
                let state = *state_stack.last().unwrap();
                //println!("{} {:?}({}) {:?} {:?}", state, current, i, state_stack, symbol_stack);
                if eof {
                    match state {
                        #( #eof_keys => {#eof_values}, )*
                        _ => {return Err(langen::errors::ParserError::UnexpectedEnd)}
                    }
                } else {
                    match state {
                        #( #actions_keys => {#actions_values}, )*
                        _ => {return Err(langen::errors::ParserError::InvalidSymbol(i))}
                    }
                }
            }
        }
    }
}

fn generate_parser_action(
    grammar: &Grammar,
    table: &ParserTable,
    state: &(usize, ParserSymbol),
    symbol_fields: &HashMap<Ident, Fields>,
    return_code: &TokenStream,
) -> TokenStream {
    match table.action_table.get(state).expect("b") {
        crate::parser::Action::Shift(next_state) => {
            quote! {
                state_stack.push(#next_state);
                symbol_stack.push(current.clone());
                i+=1;
                if let Some(x) = iter.next() {
                    current = x;
                } else {
                    eof = true;
                }
                continue;
            }
        }
        crate::parser::Action::Reduce(rule_index) => {
            let num_removed = grammar.rules[*rule_index].1.len();
            let to_ident = grammar.symbols[grammar.rules[*rule_index].0]
                .get_ident()
                .unwrap();
            let from_idents = grammar.rules[*rule_index]
                .1
                .iter()
                .map(|e| &grammar.symbols[*e]);
            let span = to_ident.span();
            let mut get_values_code = vec![];
            let mut var_names = vec![];
            for (i, ident) in from_idents.enumerate() {
                let name = ident
                    .get_ident()
                    .expect("Everything at this stage has an ident");
                let fields = &symbol_fields[&name];
                let var_name = format_ident!("v{}", i);
                var_names.push(var_name.clone());
                get_values_code.push(match fields {
                    Fields::Named(fields) => {
                        let mut field_names = vec![];
                        let mut names = vec![];
                        for (i, field) in fields.named.iter().enumerate() {
                            names.push(format_ident!("f{}", i));
                            field_names.push(field.ident.as_ref().expect("This should be named"))
                        }
                        quote_spanned!(span=>
                            let #var_name = if let Some(Self::#name{#(#field_names: #names),*}) = symbol_stack.pop() {
                                (#(#names),*,)
                            } else {
                                unreachable!()
                            }
                        )
                    },
                    Fields::Unnamed(fields) => {
                        let mut names = vec![];
                        for i in 0..fields.unnamed.len() {
                            names.push(format_ident!("f{}", i));
                        }
                        quote_spanned!(span=>
                            let #var_name = if let Some(Self::#name(#(#names),*)) = symbol_stack.pop() {
                                (#(#names),*,)
                            } else {
                                unreachable!()
                            }
                        )
                    },
                    Fields::Unit => quote_spanned!(span=>
                        let #var_name = if let Some(Self::#name) = symbol_stack.pop() {
                            ()
                        } else {
                            unreachable!()
                        }
                    ),
                });
            }
            get_values_code.reverse();

            let mut result_names = vec![];
            for i in 0..symbol_fields[&to_ident].len() {
                result_names.push(format_ident!("r{}", i))
            }

            let new_symbol = match &symbol_fields[&to_ident] {
                Fields::Named(fields) => {
                    let field_names = fields
                        .named
                        .iter()
                        .map(|f| f.ident.as_ref().expect("This is named"));
                    quote_spanned!(span=> Self::#to_ident{#(#field_names: #result_names),*})
                }
                Fields::Unnamed(_) => quote_spanned!(span=> Self::#to_ident(#(#result_names),*)),
                Fields::Unit => quote_spanned!(span=> Self::#to_ident),
            };

            let closure = &grammar.rules[*rule_index].2;

            let mut goto_results_keys = vec![];
            let mut goto_results_values = vec![];
            for (k, v) in &table.goto_table {
                if k.1 == grammar.symbols[grammar.rules[*rule_index].0] {
                    goto_results_keys.push(k.0);
                    goto_results_values.push(v);
                }
            }
            quote_spanned! {span=>
                state_stack.truncate(state_stack.len()-#num_removed);
                #(#get_values_code);*;
                let (#(#result_names),*,) = ((#closure)(#(#var_names),*)).map_err(|e: String| langen::errors::ParserError::AstError(e))?;
                symbol_stack.push(#new_symbol);
                state_stack.push(match *state_stack.last().unwrap() {
                    #( #goto_results_keys => #goto_results_values, )*
                    _ => {return Err(langen::errors::ParserError::InvalidSymbol(i))}
                });
            }
        }
        crate::parser::Action::Accept => quote! {
            #return_code
        },
    }
}
