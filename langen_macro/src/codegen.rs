use std::collections::HashMap;

use crate::{finite_automaton::FiniteAutomaton, parser::*};
use proc_macro2::TokenStream;
use quote::*;

pub fn generate_scan(dfa: FiniteAutomaton<(), Option<char>>) -> TokenStream {
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
    let mut end_states = Vec::new();
    let mut end_tokens = Vec::new();
    let mut end_states_ignored = Vec::new();
    for state in dfa.end_states {
        match state.token {
            Some(token) => {
                end_states.push(state.state);
                end_tokens.push(token);
            }
            None => {
                end_states_ignored.push(state.state);
            }
        }
    }
    let start_state = dfa.start_state;
    quote! {
        pub fn scan(input: &str) -> Result<Vec<Self>, langen::errors::LexerError> {
            #( static #identifiers: [usize; #num_states]  = [#tables]; )*
            let mut tokens = Vec::new();
            let mut last = #num_states;
            let mut current = #start_state;
            for (i, c) in input.chars().enumerate() {
                current = match c {
                    #( #chars => #identifiers[current], )*
                    _ => {return Err(langen::errors::LexerError::InvalidChar(i))}
                };
                if current == #num_states {
                    match last {
                        #( #end_states => tokens.push(Self::#end_tokens), )*
                        #( #end_states_ignored => {}, )*
                    _ => {return Err(langen::errors::LexerError::InvalidChar(i-1))}
                    };
                    current = match c {
                        #( #chars => #identifiers[#start_state], )*
                        _ => {return Err(langen::errors::LexerError::InvalidChar(i))}
                    };
                }
                last = current;
            }
            match current {
                #( #end_states => tokens.push(Self::#end_tokens), )*
                #( #end_states_ignored => {}, )*
            _ => {return Err(langen::errors::LexerError::IncompleteToken)}
            };
            Ok(tokens)
        }
    }
}

pub fn generate_check(grammar: &Grammar, table: &ParserTable) -> TokenStream {
    let mut actions = vec![HashMap::new(); table.num_states];
    let mut eof_actions = HashMap::new();
    for k in table.action_table.keys() {
        match &k.1 {
            ParserSymbol::Symbol(ident) => {
                actions[k.0].insert(ident, generate_parser_action(grammar, table, k))
            }
            ParserSymbol::Terminal(ident) => {
                actions[k.0].insert(ident, generate_parser_action(grammar, table, k))
            }
            ParserSymbol::Eof => eof_actions.insert(k.0, generate_parser_action(grammar, table, k)),
            ParserSymbol::Epsilon => panic!(),
            ParserSymbol::Start => panic!(),
        };
    }
    let mut actions_keys = vec![];
    let mut actions_values = vec![];
    for i in 0..table.num_states {
        actions_keys.push(i);
        let mut idents = vec![];
        let mut codes = vec![];
        for (ident, code) in &actions[i] {
            idents.push(ident);
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
        pub fn check(input: Vec<Self>) -> Result<(), langen::errors::ParserError> {
            let mut state_stack = vec![0usize];
            let mut symbol_stack = vec![];
            let mut i = 0usize;
            let mut eof = false;
            let mut current = match input.get(i){
                Some(x) => x,
                None => {return Err(langen::errors::ParserError::UnexpectedEnd);},
            };
            loop {
                let state = *state_stack.last().unwrap();
                println!("{} {:?}({}) {:?} {:?}", state, current, i, state_stack, symbol_stack);
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
) -> TokenStream {
    match table.action_table.get(state).expect("b") {
        crate::parser::Action::Shift(next_state) => {
            let token = state.1.get_ident().unwrap();
            quote! {
                println!("Shift({})", #next_state);
                state_stack.push(#next_state);
                symbol_stack.push(Self::#token);
                i+=1;
                if let Some(x) = input.get(i) {
                    current = x;
                } else {
                    eof = true;
                }
            }
        }
        crate::parser::Action::Reduce(rule_index) => {
            let num_removed = grammar.rules[*rule_index].1.len();
            let token = grammar.symbols[grammar.rules[*rule_index].0]
                .get_ident()
                .unwrap();
            let mut goto_results_keys = vec![];
            let mut goto_results_values = vec![];
            for (k, v) in &table.goto_table {
                if k.1 == grammar.symbols[grammar.rules[*rule_index].0] {
                    goto_results_keys.push(k.0);
                    goto_results_values.push(v);
                }
            }
            quote! {
                println!("Reduce({}), Goto()", #rule_index);
                state_stack.truncate(state_stack.len()-#num_removed);
                symbol_stack.truncate(symbol_stack.len()-#num_removed);
                symbol_stack.push(Self::#token);
                state_stack.push(match *state_stack.last().unwrap() {
                    #( #goto_results_keys => #goto_results_values, )*
                    _ => {return Err(langen::errors::ParserError::InvalidSymbol(i))}
                });
            }
        }
        crate::parser::Action::Accept => quote! {return Ok(());},
    }
}
