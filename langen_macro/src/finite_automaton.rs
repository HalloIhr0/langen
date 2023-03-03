use std::cmp::min;
use std::fmt;

use regex_syntax::hir::Hir;
use regex_syntax::hir::*;
use syn::Ident;

pub struct StateTransition {
    pub from_state: u32,
    pub transition: Option<char>,
    pub to_state: u32,
}

#[derive(Clone)]
pub struct EndState {
    pub state: u32,
    pub token: Option<Ident>,
}

impl EndState {
    fn new(state: u32) -> Self {
        Self { state, token: None }
    }
}

pub struct FiniteAutomaton {
    pub num_states: u32,
    pub start_state: u32,
    pub end_states: Vec<EndState>,
    pub transitions: Vec<StateTransition>,
}

impl FiniteAutomaton {
    /// Converts a regex to an nfa using Thompson's construction
    /// For regex ranges (for example [a-z] or .) only ascii characters are allowed
    pub fn from_regex(regex: &Hir) -> Option<Self> {
        match regex.kind() {
            HirKind::Empty => Some(Self {
                num_states: 1,
                start_state: 0,
                end_states: vec![EndState::new(0)],
                transitions: vec![],
            }),
            HirKind::Literal(Literal::Unicode(lit)) => Some(Self {
                num_states: 2,
                start_state: 0,
                end_states: vec![EndState::new(1)],
                transitions: vec![StateTransition {
                    from_state: 0,
                    transition: Some(*lit),
                    to_state: 1,
                }],
            }),
            HirKind::Class(Class::Unicode(class)) => {
                let mut result = Self {
                    num_states: 2,
                    start_state: 0,
                    end_states: vec![EndState::new(1)],
                    transitions: vec![],
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
            HirKind::Repetition(repetition) => Self::handle_repititon(repetition),
            HirKind::Group(group) => Self::from_regex(&group.hir),
            HirKind::Concat(elements) => {
                let mut result = Self {
                    num_states: 1,
                    start_state: 0,
                    end_states: vec![EndState::new(0)],
                    transitions: vec![],
                };
                for part in elements {
                    let part_automaton = Self::from_regex(part)?;
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
                let mut result = Self {
                    num_states: 2,
                    start_state: 0,
                    end_states: vec![EndState::new(1)],
                    transitions: vec![],
                };
                for part in elements {
                    let part_automaton = Self::from_regex(part)?;
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

    /// Adds the states and transitions of "other" to "self"
    /// They will always be appended after "self" (every state from "other" is offset by num_nodes of "self")
    /// This function creates no connections between them
    pub fn add_automaton(&mut self, other: &Self) {
        for transition in &other.transitions {
            self.transitions.push(StateTransition {
                from_state: transition.from_state + self.num_states,
                transition: transition.transition,
                to_state: transition.to_state + self.num_states,
            })
        }
        self.num_states += other.num_states;
    }

    fn handle_repititon(repitition: &Repetition) -> Option<Self> {
        match repitition.kind {
            RepetitionKind::ZeroOrOne => {
                let mut result = Self {
                    num_states: 2,
                    start_state: 0,
                    end_states: vec![EndState::new(1)],
                    transitions: vec![StateTransition {
                        from_state: 0,
                        transition: None,
                        to_state: 1,
                    }],
                };
                let part_automaton = Self::from_regex(&repitition.hir)?;
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
                let mut result = Self {
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
                };
                let part_automaton = Self::from_regex(&repitition.hir)?;
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
                let mut result = Self {
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
                };
                let part_automaton = Self::from_regex(&repitition.hir)?;
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
}

impl fmt::Display for FiniteAutomaton {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#={}\ts={}\te={{", self.num_states, self.start_state)?;
        for state in &self.end_states {
            write!(f, "{}({:?}), ", state.state, state.token)?;
        }
        write!(f, "}}")?;

        for transition in &self.transitions {
            write!(
                f,
                "\n{} =={:?}=> {}",
                transition.from_state, transition.transition, transition.to_state
            )?;
        }
        Ok(())
    }
}
