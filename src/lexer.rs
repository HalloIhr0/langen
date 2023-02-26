use regex_syntax::Parser;
use syn::Ident;

use crate::finite_automaton::*;

pub struct TokenVariant {
    pub name: Ident,
    pub regex: String,
}

pub fn create_nfa(tokens: Vec<TokenVariant>) -> FiniteAutomaton {
    let mut result = FiniteAutomaton {
        num_states: 1,
        start_state: 0,
        end_states: vec![],
        transitions: vec![],
    };

    for token in tokens {
        let part =
            FiniteAutomaton::from_regex(&Parser::new().parse(&token.regex).unwrap_or_else(|_| {
                panic!(
                    "Invalid invalid regex \"{}\" for token \"{}\"",
                    token.regex, token.name
                )
            }))
            .unwrap_or_else(|| {
                panic!(
                    "Couldnt parse regex \"{}\" for token \"{}\"",
                    token.regex, token.name
                )
            });
        if part.end_states.len() != 1 {
            panic!(
                "Error in regex \"{}\" for token \"{}\"",
                token.regex, token.name
            )
        }
        let old_num_states = result.num_states;
        result.add_automaton(&part);
        result.transitions.push(StateTransition {
            from_state: 0,
            transition: None,
            to_state: part.start_state + old_num_states,
        });
        result.end_states.push(EndState {
            state: part.end_states[0].state + old_num_states,
            token: Some(token.name),
        })
    }
    result
}
