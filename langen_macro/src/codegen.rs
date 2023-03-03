use std::collections::HashMap;

use crate::finite_automaton::FiniteAutomaton;
use proc_macro2::TokenStream;
use quote::*;

pub fn generate_scan(dfa: FiniteAutomaton) -> TokenStream {
    let mut transitions = HashMap::new();
    let num_states = dfa.num_states;
    for transition in &dfa.transitions {
        transitions
            .entry(transition.transition.unwrap())
            .or_insert_with(|| vec![num_states; num_states as usize]);
        transitions
            .get_mut(&transition.transition.unwrap())
            .unwrap()[transition.from_state as usize] = transition.to_state;
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
    let num_states_usize = num_states as usize;
    let start_state = dfa.start_state;
    let start_state_usize = dfa.start_state as usize;
    quote! {
        fn scan(input: &str) -> Result<Vec<Self>, langen::errors::LexerError> {
            #( static #identifiers: [u32; #num_states_usize]  = [#tables]; )*
            let mut tokens = Vec::new();
            let mut last = #num_states;
            let mut current = #start_state;
            for (i, c) in input.chars().enumerate() {
                current = match c {
                    #( #chars => #identifiers[current as usize], )*
                    _ => {return Err(langen::errors::LexerError::InvalidChar(i))}
                };
                if current == #num_states {
                    match last {
                        #( #end_states => tokens.push(Self::#end_tokens), )*
                        #( #end_states_ignored => {}, )*
                    _ => {return Err(langen::errors::LexerError::InvalidChar(i-1))}
                    };
                    current = match c {
                        #( #chars => #identifiers[#start_state_usize], )*
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
