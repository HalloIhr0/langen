use std::collections::{BTreeSet, HashMap};

use regex_syntax::Parser;
use syn::Ident;

use crate::finite_automaton::*;

pub struct TokenVariant {
    pub name: Ident,
    pub regex: String,
}

pub fn create_finite_automaton(tokens: Vec<TokenVariant>) -> FiniteAutomaton {
    let nfa = create_nfa(&tokens);
    let dfa = convert_nfa_to_dfa(&nfa, &tokens);
    dfa
}

fn create_nfa(tokens: &Vec<TokenVariant>) -> FiniteAutomaton {
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
            token: Some(token.name.clone()),
        })
    }
    result
}

/// Converts an nfa (nondeterministic finite automaton) to an dfa (deterministic finite automaton) using powerset construction the powerset construction
/// The token vector is used, to determine which tokens have priority over other tokens. The tokens that appear first have priority
fn convert_nfa_to_dfa(automaton: &FiniteAutomaton, tokens: &Vec<TokenVariant>) -> FiniteAutomaton {
    let mut result = FiniteAutomaton {
        num_states: 1,
        start_state: 0,
        end_states: vec![],
        transitions: vec![],
    };
    let new_start = epsilon_closure_single(automaton, &automaton.start_state);
    let mut states = HashMap::<BTreeSet<u32>, u32>::new();
    states.insert(new_start.clone(), 0);
    let mut active = vec![new_start];

    while !active.is_empty() {
        let current = active.pop().unwrap();
        for symbol in get_possible_inputs(automaton, &current) {
            let next = epsilon_closure(automaton, &move_result(automaton, &current, symbol));
            if !states.contains_key(&next) {
                states.insert(next.clone(), states.len() as u32);
                active.push(next.clone());
            }
            result.transitions.push(StateTransition {
                from_state: *states.get(&current).unwrap(),
                transition: Some(symbol),
                to_state: *states.get(&next).unwrap(),
            });
        }
    }
    result.num_states = states.len() as u32;

    // Maps dfa states to nfa ned_states
    let mut new_ends: HashMap<u32, Option<Ident>> = HashMap::new();
    for (k, v) in &states {
        for end_state in automaton.end_states.clone() {
            if k.contains(&end_state.state) {
                let new_token = end_state.token;
                if new_ends.contains_key(v) {
                    let old_token = new_ends.get(v).unwrap();
                    // None token (skip) always gets priority
                    if old_token.is_some() {
                        if let Some(new) = new_token {
                            let old_token = old_token.clone().unwrap();
                            for token in tokens {
                                if token.name == old_token {
                                    break;
                                } else if token.name == new {
                                    new_ends.insert(*v, Some(new));
                                    break;
                                }
                            }
                        } else {
                            new_ends.insert(*v, new_token);
                        }
                    }
                } else {
                    new_ends.insert(*v, new_token);
                }
            }
        }
    }

    for (state, end) in new_ends {
        result.end_states.push(EndState { state, token: end })
    }

    result
}

fn get_possible_inputs(automaton: &FiniteAutomaton, states: &BTreeSet<u32>) -> Vec<char> {
    let mut result = Vec::new();
    for transition in &automaton.transitions {
        if states.contains(&transition.from_state) {
            if let Some(c) = transition.transition {
                result.push(c);
            }
        }
    }
    result
}

fn move_result(automaton: &FiniteAutomaton, states: &BTreeSet<u32>, symbol: char) -> BTreeSet<u32> {
    let mut result = BTreeSet::new();
    for transition in &automaton.transitions {
        if states.contains(&transition.from_state) && Some(symbol) == transition.transition {
            result.insert(transition.to_state);
        }
    }
    result
}

fn epsilon_closure(automaton: &FiniteAutomaton, states: &BTreeSet<u32>) -> BTreeSet<u32> {
    let mut result = BTreeSet::new();
    for state in states {
        result.extend(epsilon_closure_single(automaton, state));
    }
    result
}

fn epsilon_closure_single(automaton: &FiniteAutomaton, state: &u32) -> BTreeSet<u32> {
    let mut result = BTreeSet::from([*state]);
    let mut active = vec![*state];
    while !active.is_empty() {
        let current = active.pop().unwrap();
        for transition in &automaton.transitions {
            if transition.from_state == current
                && transition.transition.is_none()
                && !result.contains(&transition.to_state)
            {
                result.insert(transition.to_state);
                active.push(transition.to_state);
            }
        }
    }
    result
}
