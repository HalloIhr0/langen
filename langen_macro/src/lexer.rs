use std::cmp::min;
use std::collections::{BTreeSet, HashMap};

use proc_macro2::Ident;
use regex_syntax::hir::Hir;
use regex_syntax::hir::*;
use regex_syntax::Parser;

use crate::finite_automaton::*;

pub struct TokenVariant {
    pub name: Ident,
    pub regex: String,
    pub ignore: bool,
}

pub fn create_finite_automaton(tokens: Vec<TokenVariant>) -> FiniteAutomaton<(), Option<char>> {
    let nfa = create_nfa(&tokens);
    convert_nfa_to_dfa(&nfa, &tokens)
}

fn create_nfa(tokens: &Vec<TokenVariant>) -> FiniteAutomaton<(), Option<char>> {
    let mut result = FiniteAutomaton {
        num_states: 1,
        start_state: 0,
        end_states: vec![],
        transitions: vec![],
        state_info: vec![],
    };

    for token in tokens {
        let part = from_regex(&Parser::new().parse(&token.regex).unwrap_or_else(|_| {
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
            token: match token.ignore {
                true => None,
                false => Some(token.name.clone()),
            },
        })
    }
    result
}

/// Converts a regex to an nfa using Thompson's construction
/// For regex ranges (for example [a-z] or .) only ascii characters are allowed
fn from_regex(regex: &Hir) -> Option<FiniteAutomaton<(), Option<char>>> {
    match regex.kind() {
        HirKind::Empty => Some(FiniteAutomaton {
            num_states: 1,
            start_state: 0,
            end_states: vec![EndState::new(0)],
            transitions: vec![],
            state_info: vec![],
        }),
        HirKind::Literal(Literal::Unicode(lit)) => Some(FiniteAutomaton {
            num_states: 2,
            start_state: 0,
            end_states: vec![EndState::new(1)],
            transitions: vec![StateTransition {
                from_state: 0,
                transition: Some(*lit),
                to_state: 1,
            }],
            state_info: vec![],
        }),
        HirKind::Class(Class::Unicode(class)) => {
            let mut result = FiniteAutomaton {
                num_states: 2,
                start_state: 0,
                end_states: vec![EndState::new(1)],
                transitions: vec![],
                state_info: vec![],
            };
            for range in class.iter() {
                let start = min(range.start(), char::from_u32(127).unwrap());
                let end = min(range.end(), char::from_u32(127).unwrap());
                for item in start..=end {
                    result.transitions.push(StateTransition {
                        from_state: 0,
                        transition: Some(item),
                        to_state: 1,
                    });
                }
            }
            Some(result)
        }
        HirKind::Anchor(_) => todo!(),
        HirKind::WordBoundary(_) => todo!(),
        HirKind::Repetition(repetition) => handle_repititon(repetition),
        HirKind::Group(group) => from_regex(&group.hir),
        HirKind::Concat(elements) => {
            let mut result = FiniteAutomaton {
                num_states: 1,
                start_state: 0,
                end_states: vec![EndState::new(0)],
                transitions: vec![],
                state_info: vec![],
            };
            for part in elements {
                let part_automaton = from_regex(part)?;
                if part_automaton.end_states.len() != 1 {
                    return None;
                }
                let old_num_states = result.num_states;
                result.add_automaton(&part_automaton);
                result.transitions.push(StateTransition {
                    from_state: result.end_states[0].state,
                    transition: None,
                    to_state: part_automaton.start_state + old_num_states,
                });
                result.end_states = vec![EndState::new(
                    part_automaton.end_states[0].state + old_num_states,
                )];
            }
            Some(result)
        }
        HirKind::Alternation(elements) => {
            let mut result = FiniteAutomaton {
                num_states: 2,
                start_state: 0,
                end_states: vec![EndState::new(1)],
                transitions: vec![],
                state_info: vec![],
            };
            for part in elements {
                let part_automaton = from_regex(part)?;
                if part_automaton.end_states.len() != 1 {
                    return None;
                }
                let old_num_states = result.num_states;
                result.add_automaton(&part_automaton);
                result.transitions.push(StateTransition {
                    from_state: 0,
                    transition: None,
                    to_state: part_automaton.start_state + old_num_states,
                });
                result.transitions.push(StateTransition {
                    from_state: part_automaton.end_states[0].state + old_num_states,
                    transition: None,
                    to_state: 1,
                });
            }
            Some(result)
        }
        _ => None,
    }
}

fn handle_repititon(repitition: &Repetition) -> Option<FiniteAutomaton<(), Option<char>>> {
    match repitition.kind {
        RepetitionKind::ZeroOrOne => {
            let mut result = FiniteAutomaton {
                num_states: 2,
                start_state: 0,
                end_states: vec![EndState::new(1)],
                transitions: vec![StateTransition {
                    from_state: 0,
                    transition: None,
                    to_state: 1,
                }],
                state_info: vec![],
            };
            let part_automaton = from_regex(&repitition.hir)?;
            if part_automaton.end_states.len() != 1 {
                return None;
            }
            let old_num_states = result.num_states;
            result.add_automaton(&part_automaton);
            result.transitions.push(StateTransition {
                from_state: 0,
                transition: None,
                to_state: part_automaton.start_state + old_num_states,
            });
            result.transitions.push(StateTransition {
                from_state: part_automaton.end_states[0].state + old_num_states,
                transition: None,
                to_state: 1,
            });
            Some(result)
        }
        RepetitionKind::ZeroOrMore => {
            let mut result = FiniteAutomaton {
                num_states: 4,
                start_state: 0,
                end_states: vec![EndState::new(3)],
                transitions: vec![
                    StateTransition {
                        from_state: 0,
                        transition: None,
                        to_state: 1,
                    },
                    StateTransition {
                        from_state: 1,
                        transition: None,
                        to_state: 2,
                    },
                    StateTransition {
                        from_state: 2,
                        transition: None,
                        to_state: 1,
                    },
                    StateTransition {
                        from_state: 2,
                        transition: None,
                        to_state: 3,
                    },
                ],
                state_info: vec![],
            };
            let part_automaton = from_regex(&repitition.hir)?;
            if part_automaton.end_states.len() != 1 {
                return None;
            }
            let old_num_states = result.num_states;
            result.add_automaton(&part_automaton);
            result.transitions.push(StateTransition {
                from_state: 1,
                transition: None,
                to_state: part_automaton.start_state + old_num_states,
            });
            result.transitions.push(StateTransition {
                from_state: part_automaton.end_states[0].state + old_num_states,
                transition: None,
                to_state: 2,
            });
            Some(result)
        }
        RepetitionKind::OneOrMore => {
            let mut result = FiniteAutomaton {
                num_states: 4,
                start_state: 0,
                end_states: vec![EndState::new(3)],
                transitions: vec![
                    StateTransition {
                        from_state: 0,
                        transition: None,
                        to_state: 1,
                    },
                    StateTransition {
                        from_state: 2,
                        transition: None,
                        to_state: 1,
                    },
                    StateTransition {
                        from_state: 2,
                        transition: None,
                        to_state: 3,
                    },
                ],
                state_info: vec![],
            };
            let part_automaton = from_regex(&repitition.hir)?;
            if part_automaton.end_states.len() != 1 {
                return None;
            }
            let old_num_states = result.num_states;
            result.add_automaton(&part_automaton);
            result.transitions.push(StateTransition {
                from_state: 1,
                transition: None,
                to_state: part_automaton.start_state + old_num_states,
            });
            result.transitions.push(StateTransition {
                from_state: part_automaton.end_states[0].state + old_num_states,
                transition: None,
                to_state: 2,
            });
            Some(result)
        }
        RepetitionKind::Range(_) => todo!(),
    }
}

/// Converts an nfa (nondeterministic finite automaton) to an dfa (deterministic finite automaton) using powerset construction the powerset construction
/// The token vector is used, to determine which tokens have priority over other tokens. The tokens that appear first have priority
fn convert_nfa_to_dfa(
    automaton: &FiniteAutomaton<(), Option<char>>,
    tokens: &Vec<TokenVariant>,
) -> FiniteAutomaton<(), Option<char>> {
    let mut result = FiniteAutomaton {
        num_states: 1,
        start_state: 0,
        end_states: vec![],
        transitions: vec![],
        state_info: vec![],
    };
    let new_start = epsilon_closure_single(automaton, &automaton.start_state);
    let mut states = HashMap::<BTreeSet<usize>, usize>::new();
    states.insert(new_start.clone(), 0);
    let mut active = vec![new_start];

    while let Some(current) = active.pop() {
        for symbol in get_possible_inputs(automaton, &current) {
            let next = epsilon_closure(automaton, &move_result(automaton, &current, symbol));
            if !states.contains_key(&next) {
                states.insert(next.clone(), states.len());
                active.push(next.clone());
            }
            result.transitions.push(StateTransition {
                from_state: *states.get(&current).unwrap(),
                transition: Some(symbol),
                to_state: *states.get(&next).unwrap(),
            });
        }
    }
    result.num_states = states.len();

    // Maps dfa states to nfa ned_states
    let mut new_ends: HashMap<usize, Option<Ident>> = HashMap::new();
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

fn get_possible_inputs(
    automaton: &FiniteAutomaton<(), Option<char>>,
    states: &BTreeSet<usize>,
) -> Vec<char> {
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

fn move_result(
    automaton: &FiniteAutomaton<(), Option<char>>,
    states: &BTreeSet<usize>,
    symbol: char,
) -> BTreeSet<usize> {
    let mut result = BTreeSet::new();
    for transition in &automaton.transitions {
        if states.contains(&transition.from_state) && Some(symbol) == transition.transition {
            result.insert(transition.to_state);
        }
    }
    result
}

fn epsilon_closure(
    automaton: &FiniteAutomaton<(), Option<char>>,
    states: &BTreeSet<usize>,
) -> BTreeSet<usize> {
    let mut result = BTreeSet::new();
    for state in states {
        result.extend(epsilon_closure_single(automaton, state));
    }
    result
}

fn epsilon_closure_single(
    automaton: &FiniteAutomaton<(), Option<char>>,
    state: &usize,
) -> BTreeSet<usize> {
    let mut result = BTreeSet::from([*state]);
    let mut active = vec![*state];
    while let Some(current) = active.pop() {
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
