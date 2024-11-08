use std::{collections::HashSet, vec};

use regex_syntax::hir::{Class, Hir};
use syn::Ident;

use crate::Token;

#[derive(PartialEq, Debug)]
enum Symbol {
    // TODO
    // Range { start: char, end: char },
    Literal(char),
    Epsilon,
}

#[derive(Debug)]
struct Transition {
    from: usize,
    to: usize,
    symbol: Symbol,
}

impl Transition {
    fn offset(&mut self, offset: usize) {
        self.from += offset;
        self.to += offset;
    }
}

#[derive(Clone, Debug)]
struct EndState {
    state: usize,
    token: Ident,
}

impl PartialEq for EndState {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state
    }
}

#[derive(Debug)]
pub struct Automaton {
    num_states: usize,
    // Start state will always be 0
    end_states: Vec<EndState>,
    transitions: Vec<Transition>,
}

impl Automaton {
    pub fn from_tokens(tokens: &[Token]) -> Self {
        let mut automaton = Self {
            num_states: 1,
            end_states: vec![],
            transitions: vec![],
        };
        for token in tokens {
            let sub = Self::thompsons(&token.regex, &token.ident);
            let start = automaton.append(sub);
            automaton.transitions.push(Transition {
                from: 0,
                to: start,
                symbol: Symbol::Epsilon,
            });
        }
        automaton
    }

    pub fn to_dfa(self) -> Self {
        // Powerset
        let start = self.closure(&HashSet::from([0]));
        let mut automaton = Self {
            num_states: 1,
            end_states: if let Some(end_state) = self.get_end_state(&start) {
                vec![EndState {
                    state: 0,
                    token: end_state.token.clone(),
                }]
            } else {
                vec![]
            },
            transitions: vec![],
        };
        let mut states = vec![start];
        let mut queue = vec![0];

        while let Some(i) = queue.pop() {
            for symbol in self.get_possible_symbols(&states[i]) {
                let connected = self.closure(&self.transition(&states[i], symbol));
                if let Some(new_i) = states.iter().position(|e| *e == connected) {
                    automaton.transitions.push(Transition {
                        from: i,
                        to: new_i,
                        symbol: Symbol::Literal(symbol),
                    });
                } else {
                    let new_i = automaton.num_states;
                    automaton.transitions.push(Transition {
                        from: i,
                        to: new_i,
                        symbol: Symbol::Literal(symbol),
                    });
                    if let Some(end_state) = self.get_end_state(&connected) {
                        automaton.end_states.push(EndState {
                            state: new_i,
                            token: end_state.token.clone(),
                        });
                    }
                    states.push(connected);
                    queue.push(new_i);
                    automaton.num_states += 1;
                }
            }
        }

        automaton
    }

    pub fn to_graphvis(&self) -> String {
        let mut result: String = "digraph {".into();
        for i in 0..self.num_states {
            result.push_str(&format!(
                "{i}[label=\"{}\"];",
                self.end_states
                    .iter()
                    .find(|end_state| end_state.state == i)
                    .map(|end_state| end_state.token.to_string())
                    .unwrap_or(String::new())
            ));
        }
        for transition in &self.transitions {
            result.push_str(&format!(
                "{}->{}[label=\"{}\"];",
                transition.from,
                transition.to,
                match transition.symbol {
                    Symbol::Literal(c) => c,
                    Symbol::Epsilon => 'ε',
                }
            ));
        }
        result.push('}');
        result
    }

    fn thompsons(regex: &Hir, ident: &Ident) -> Self {
        match regex.kind() {
            regex_syntax::hir::HirKind::Empty => Self {
                num_states: 1,
                end_states: vec![EndState {
                    state: 0,
                    token: ident.clone(),
                }],
                transitions: vec![],
            },
            regex_syntax::hir::HirKind::Literal(literal) => {
                let str = String::from_utf8(literal.0.to_vec()).expect("Invalid unicode in regex. Since this comes from a string literal, you found a rust bug?");
                let mut automaton = Self {
                    num_states: str.len() + 1,
                    end_states: vec![EndState {
                        state: str.len(),
                        token: ident.clone(),
                    }],
                    transitions: vec![],
                };
                for (i, char) in str.chars().enumerate() {
                    automaton.transitions.push(Transition {
                        from: i,
                        to: i + 1,
                        symbol: Symbol::Literal(char),
                    });
                }
                automaton
            }
            regex_syntax::hir::HirKind::Class(Class::Unicode(class)) => {
                let mut automaton = Self {
                    num_states: 2,
                    end_states: vec![EndState {
                        state: 1,
                        token: ident.clone(),
                    }],
                    transitions: vec![],
                };
                for range in class.ranges() {
                    let start = range.start();
                    let end = range.end().min('\x7f');
                    for c in start..=end {
                        automaton.transitions.push(Transition {
                            from: 0,
                            to: 1,
                            symbol: Symbol::Literal(c),
                        });
                    }
                }
                automaton
            }
            regex_syntax::hir::HirKind::Class(Class::Bytes(_)) => {
                panic!("Can only use unicode classes")
            }
            regex_syntax::hir::HirKind::Look(look) => todo!(),
            regex_syntax::hir::HirKind::Repetition(repetition) => {
                match (repetition.min, repetition.max, repetition.greedy) {
                    (min, _, false) => {
                        // non-greedy will always finish match after min, so max doesn't matter
                        let mut automaton = Self {
                            num_states: 0,
                            end_states: vec![],
                            transitions: vec![],
                        };
                        for _ in 0..min {
                            let sub = Self::thompsons(&repetition.sub, ident);
                            let old_end_states = automaton.end_states.clone();
                            let start = automaton.append(sub);
                            automaton.end_states.retain(|end_state| {
                                if old_end_states.contains(end_state) {
                                    automaton.transitions.push(Transition {
                                        from: end_state.state,
                                        to: start,
                                        symbol: Symbol::Epsilon,
                                    });
                                    false
                                } else {
                                    true
                                }
                            });
                        }
                        automaton
                    }
                    (min, Some(max), true) => {
                        // We don't need repetition for this
                        let mut automaton = Self {
                            num_states: 1,
                            end_states: vec![EndState {
                                state: 0,
                                token: ident.clone(),
                            }],
                            transitions: vec![],
                        };
                        for i in 0..max {
                            let sub = Self::thompsons(&repetition.sub, ident);
                            let old_end_states = automaton.end_states.clone();
                            let start = automaton.append(sub);
                            automaton.end_states.retain(|end_state| {
                                if old_end_states.contains(end_state) {
                                    automaton.transitions.push(Transition {
                                        from: end_state.state,
                                        to: start,
                                        symbol: Symbol::Epsilon,
                                    });
                                    i >= min
                                } else {
                                    true
                                }
                            });
                        }
                        automaton
                    }
                    (min, None, true) => {
                        // Force min steps, then insert loop
                        let mut automaton = Self {
                            num_states: 1,
                            end_states: vec![EndState {
                                state: 0,
                                token: ident.clone(),
                            }],
                            transitions: vec![],
                        };
                        for i in 0..min {
                            let sub = Self::thompsons(&repetition.sub, ident);
                            let old_end_states = automaton.end_states.clone();
                            let start = automaton.append(sub);
                            automaton.end_states.retain(|end_state| {
                                if old_end_states.contains(end_state) {
                                    automaton.transitions.push(Transition {
                                        from: end_state.state,
                                        to: start,
                                        symbol: Symbol::Epsilon,
                                    });
                                    i != min - 1
                                } else {
                                    true
                                }
                            });
                        }
                        let sub = Self::thompsons(&repetition.sub, ident);
                        let start = automaton.append(sub);
                        for end_state in &automaton.end_states {
                            automaton.transitions.push(Transition {
                                from: end_state.state,
                                to: start,
                                symbol: Symbol::Epsilon,
                            });
                        }
                        automaton
                    }
                }
            }
            regex_syntax::hir::HirKind::Capture(capture) => todo!(),
            regex_syntax::hir::HirKind::Concat(vec) => {
                let mut automaton = Self {
                    num_states: 0,
                    end_states: vec![],
                    transitions: vec![],
                };
                for sub in vec {
                    let sub = Self::thompsons(sub, ident);
                    let old_end_states = automaton.end_states.clone();
                    let start = automaton.append(sub);
                    automaton.end_states.retain(|end_state| {
                        if old_end_states.contains(end_state) {
                            automaton.transitions.push(Transition {
                                from: end_state.state,
                                to: start,
                                symbol: Symbol::Epsilon,
                            });
                            false
                        } else {
                            true
                        }
                    });
                }
                automaton
            }
            regex_syntax::hir::HirKind::Alternation(vec) => {
                let mut automaton = Self {
                    num_states: 1,
                    end_states: vec![],
                    transitions: vec![],
                };
                for sub in vec {
                    let sub = Self::thompsons(sub, ident);
                    let start = automaton.append(sub);
                    automaton.transitions.push(Transition {
                        from: 0,
                        to: start,
                        symbol: Symbol::Epsilon,
                    });
                }
                automaton
            }
        }
    }

    /// Return index of new start state
    fn append(&mut self, other: Self) -> usize {
        let offset = self.num_states;
        for mut end_state in other.end_states {
            end_state.state += offset;
            self.end_states.push(end_state);
        }
        for mut transition in other.transitions {
            transition.offset(offset);
            self.transitions.push(transition);
        }
        self.num_states += other.num_states;
        offset
    }

    fn get_end_state(&self, states: &HashSet<usize>) -> Option<&EndState> {
        self.end_states
            .iter()
            .find(|end_state| states.contains(&end_state.state))
    }

    fn get_possible_symbols(&self, states: &HashSet<usize>) -> HashSet<char> {
        let mut result = HashSet::new();
        for transition in &self.transitions {
            if states.contains(&transition.from) {
                // This should always be called on closured states, so epsilon can be ignored
                if let Symbol::Literal(c) = transition.symbol {
                    result.insert(c);
                }
            }
        }
        result
    }

    fn transition(&self, states: &HashSet<usize>, symbol: char) -> HashSet<usize> {
        let mut result = HashSet::new();
        for transition in &self.transitions {
            // This should always be called on closured states, so epsilon can be ignored
            if states.contains(&transition.from) && transition.symbol == Symbol::Literal(symbol) {
                result.insert(transition.to);
            }
        }
        result
    }

    fn closure(&self, states: &HashSet<usize>) -> HashSet<usize> {
        let mut result = HashSet::new();
        let mut queue = states.iter().copied().collect::<Vec<_>>();
        while let Some(state) = queue.pop() {
            result.insert(state);
            for transition in &self.transitions {
                if transition.from == state && transition.symbol == Symbol::Epsilon {
                    let to = transition.to;
                    if !(result.contains(&to) || queue.contains(&to)) {
                        queue.push(to);
                    }
                }
            }
        }
        result
    }
}
