use std::fmt;

use regex_syntax::hir::Hir;
use regex_syntax::hir::*;

struct StateTransition {
    from_state: u32,
    transition: Option<char>,
    to_state: u32,
}

pub struct FiniteAutomaton {
    num_states: u32,
    start_state: u32,
    end_states: Vec<u32>,
    transitions: Vec<StateTransition>,
}

impl FiniteAutomaton {
    pub fn from_regex(regex: &Hir) -> Option<Self> {
        match regex.kind() {
            HirKind::Empty => Some(Self {
                num_states: 1,
                start_state: 0,
                end_states: vec![0],
                transitions: vec![],
            }),
            HirKind::Literal(Literal::Unicode(lit)) => Some(Self {
                num_states: 2,
                start_state: 0,
                end_states: vec![1],
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
                    end_states: vec![1],
                    transitions: vec![],
                };
                for range in class.iter() {
                    for item in range.start()..=range.end() {
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
                    end_states: vec![0],
                    transitions: vec![],
                };
                for part in elements {
                    let part_automaton = Self::from_regex(part)?;
                    if part_automaton.end_states.len() != 1 {
                        return None;
                    }
                    for transition in part_automaton.transitions {
                        result.transitions.push(StateTransition {
                            from_state: transition.from_state + result.num_states,
                            transition: transition.transition,
                            to_state: transition.to_state + result.num_states,
                        })
                    }
                    result.transitions.push(StateTransition {
                        from_state: result.end_states[0],
                        transition: None,
                        to_state: part_automaton.start_state + result.num_states,
                    });
                    result.end_states = vec![part_automaton.end_states[0] + result.num_states];
                    result.num_states += part_automaton.num_states;
                }
                Some(result)
            }
            HirKind::Alternation(elements) => {
                let mut result = Self {
                    num_states: 2,
                    start_state: 0,
                    end_states: vec![1],
                    transitions: vec![],
                };
                for part in elements {
                    let part_automaton = Self::from_regex(part)?;
                    if part_automaton.end_states.len() != 1 {
                        return None;
                    }
                    for transition in part_automaton.transitions {
                        result.transitions.push(StateTransition {
                            from_state: transition.from_state + result.num_states,
                            transition: transition.transition,
                            to_state: transition.to_state + result.num_states,
                        })
                    }
                    result.transitions.push(StateTransition {
                        from_state: 0,
                        transition: None,
                        to_state: part_automaton.start_state + result.num_states,
                    });
                    result.transitions.push(StateTransition {
                        from_state: part_automaton.end_states[0] + result.num_states,
                        transition: None,
                        to_state: 1,
                    });
                    result.num_states += part_automaton.num_states;
                }
                Some(result)
            }
            _ => None,
        }
    }

    fn handle_repititon(repitition: &Repetition) -> Option<Self> {
        match repitition.kind {
            RepetitionKind::ZeroOrOne => {
                let mut result = Self {
                    num_states: 2,
                    start_state: 0,
                    end_states: vec![1],
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
                for transition in part_automaton.transitions {
                    result.transitions.push(StateTransition {
                        from_state: transition.from_state + result.num_states,
                        transition: transition.transition,
                        to_state: transition.to_state + result.num_states,
                    })
                }
                result.transitions.push(StateTransition {
                    from_state: 0,
                    transition: None,
                    to_state: part_automaton.start_state + result.num_states,
                });
                result.transitions.push(StateTransition {
                    from_state: part_automaton.end_states[0] + result.num_states,
                    transition: None,
                    to_state: 1,
                });
                result.num_states += part_automaton.num_states;
                Some(result)
            }
            RepetitionKind::ZeroOrMore => {
                let mut result = Self {
                    num_states: 4,
                    start_state: 0,
                    end_states: vec![3],
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
                for transition in part_automaton.transitions {
                    result.transitions.push(StateTransition {
                        from_state: transition.from_state + result.num_states,
                        transition: transition.transition,
                        to_state: transition.to_state + result.num_states,
                    })
                }
                result.transitions.push(StateTransition {
                    from_state: 1,
                    transition: None,
                    to_state: part_automaton.start_state + result.num_states,
                });
                result.transitions.push(StateTransition {
                    from_state: part_automaton.end_states[0] + result.num_states,
                    transition: None,
                    to_state: 2,
                });
                result.num_states += part_automaton.num_states;
                Some(result)
            }
            RepetitionKind::OneOrMore => {
                let mut result = Self {
                    num_states: 4,
                    start_state: 0,
                    end_states: vec![3],
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
                for transition in part_automaton.transitions {
                    result.transitions.push(StateTransition {
                        from_state: transition.from_state + result.num_states,
                        transition: transition.transition,
                        to_state: transition.to_state + result.num_states,
                    })
                }
                result.transitions.push(StateTransition {
                    from_state: 1,
                    transition: None,
                    to_state: part_automaton.start_state + result.num_states,
                });
                result.transitions.push(StateTransition {
                    from_state: part_automaton.end_states[0] + result.num_states,
                    transition: None,
                    to_state: 2,
                });
                result.num_states += part_automaton.num_states;
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
            write!(f, "{}, ", state)?;
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
