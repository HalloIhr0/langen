use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
    iter,
};

use proc_macro2::Ident;

use crate::finite_automaton::*;

/// If ident is None and terminal is true, it's the eof token
#[derive(Debug, Clone, Ord, Eq, PartialOrd, PartialEq, Hash)]
pub struct ParserSymbol {
    pub ident: Option<Ident>,
    pub terminal: bool,
}

impl Display for ParserSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ident {
            Some(value) => write!(f, "{} ({})", value, self.terminal),
            None => write!(f, "START STATE ({})", self.terminal),
        }
    }
}

pub struct Grammar {
    pub symbols: Vec<ParserSymbol>,
    pub rules: Vec<(usize, Vec<usize>)>,
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, s) in self.symbols.iter().enumerate() {
            writeln!(f, "{}: {}", i, s)?;
        }
        for r in &self.rules {
            write!(f, "{} =>", r.0)?;
            for s in &r.1 {
                write!(f, " {}", s)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Ord, Eq, PartialOrd, PartialEq)]
struct Item {
    rule_index: usize,
    dot_index: usize,
}

#[derive(Debug, PartialEq)]
pub enum Action {
    Shift(usize),  // next state
    Reduce(usize), // rule by index
    Accept,
}

#[derive(Debug)]
pub struct ParserTable {
    pub action_table: HashMap<(usize, ParserSymbol), Action>,
    pub goto_table: HashMap<(usize, ParserSymbol), usize>,
    pub num_states: usize,
}

impl ParserTable {
    pub fn create(g: &mut Grammar) -> Self {
        check_start(&g);
        let graph = generate_graph(g);
        create_table(g, &graph)
    }
}

fn check_start(g: &Grammar) {
    let start_symbol = g.rules[0].0;
    let mut found_start = false;
    for (from, to) in &g.rules {
        if to.contains(&start_symbol) {
            panic!("Rule for \"{}\" can't produce the start symbol\nConsider adding a new start symbol which hast a rule that converts it to the original start symbol", g.symbols[*from].ident.as_ref().unwrap().to_string());
        }
        if *from == start_symbol {
            if found_start {
                panic!("Can't have two or more rules for the start symbol\nConsider adding a new start symbol which hast a rule that converts it to the original start symbol");
            } else {
                found_start = true;
            }
        }
    }
}

/// This function assumes that the first rule is always the starting rule
fn generate_graph(g: &Grammar) -> FiniteAutomaton<BTreeSet<Item>, ParserSymbol> {
    let start = closure(
        g,
        &BTreeSet::from([Item {
            rule_index: 0,
            dot_index: 0,
        }]),
    );
    let mut result = FiniteAutomaton {
        num_states: 1,
        start_state: 0,
        end_states: vec![],
        transitions: vec![],
        state_info: vec![start],
    };
    let mut current = 0;

    while current < result.num_states {
        let current_items = &result.state_info[current].clone();
        let mut next_symbols = BTreeSet::new();
        for item in current_items {
            if let Some(symbol) = g.rules[item.rule_index].1.get(item.dot_index) {
                next_symbols.insert(&g.symbols[*symbol]);
            }
        }
        for symbol in next_symbols {
            let goto_result = goto(g, current_items, symbol);
            if !goto_result.is_empty() {
                if !result.state_info.contains(&goto_result) {
                    result.state_info.push(goto_result);
                    result.transitions.push(StateTransition {
                        from_state: current,
                        transition: symbol.clone(),
                        to_state: result.num_states,
                    });
                    result.num_states += 1;
                } else {
                    let index = result
                        .state_info
                        .iter()
                        .position(|x| *x == goto_result)
                        .unwrap();
                    result.transitions.push(StateTransition {
                        from_state: current,
                        transition: symbol.clone(),
                        to_state: index,
                    });
                }
            }
        }
        current += 1;
    }
    result
}

/// This function assumes that the first rule is always the starting rule
fn create_table(g: &Grammar, graph: &FiniteAutomaton<BTreeSet<Item>, ParserSymbol>) -> ParserTable {
    let mut action_table = HashMap::new();
    let mut goto_table = HashMap::new();

    for transition in &graph.transitions {
        match transition.transition.terminal {
            true => {
                action_table.insert(
                    (transition.from_state, transition.transition.clone()),
                    Action::Shift(transition.to_state),
                );
            }
            false => {
                goto_table.insert(
                    (transition.from_state, transition.transition.clone()),
                    transition.to_state,
                );
            }
        };
    }
    for (i, state) in graph.state_info.iter().enumerate() {
        let last_index = 0;
        if state.contains(&Item {
            rule_index: last_index,
            dot_index: g.rules[last_index].1.len(),
        }) {
            action_table.insert(
                (
                    i,
                    ParserSymbol {
                        ident: None,
                        terminal: true,
                    },
                ),
                Action::Accept,
            );
        }
        for item in state {
            if item.dot_index == g.rules[item.rule_index].1.len() {
                for symbol in g.symbols.iter().chain(iter::once(&ParserSymbol {
                    ident: None,
                    terminal: true,
                })) {
                    if symbol.terminal {
                        if let std::collections::hash_map::Entry::Vacant(e) =
                            action_table.entry((i, symbol.clone()))
                        {
                            e.insert(Action::Reduce(item.rule_index));
                        } else {
                            // If the previous action is accept, we can safely ignore it
                            if *action_table.get(&(i, symbol.clone())).unwrap() != Action::Accept {
                                println!("Conflict");
                            }
                        }
                    }
                }
            }
        }
    }

    ParserTable {
        action_table,
        goto_table,
        num_states: graph.num_states,
    }
}

fn closure(g: &Grammar, items: &BTreeSet<Item>) -> BTreeSet<Item> {
    let mut result = BTreeSet::new();
    let mut stack = Vec::new();
    for item in items {
        result.insert(*item);
        stack.push(*item);
    }
    while !stack.is_empty() {
        let current = stack.pop().unwrap();
        if let Some(symbol) = g.rules[current.rule_index].1.get(current.dot_index) {
            let symbol = &g.symbols[*symbol];
            if !symbol.terminal {
                for (i, (rule, _)) in g.rules.iter().enumerate() {
                    if g.symbols.get(*rule).unwrap() == symbol {
                        let item = Item {
                            rule_index: i,
                            dot_index: 0,
                        };
                        if !result.contains(&item) {
                            result.insert(item);
                            stack.push(item);
                        }
                    }
                }
            }
        }
    }
    result
}

fn goto(g: &Grammar, items: &BTreeSet<Item>, next_symbol: &ParserSymbol) -> BTreeSet<Item> {
    let mut result = BTreeSet::new();
    for item in items {
        if let Some(symbol) = g.rules[item.rule_index].1.get(item.dot_index) {
            let symbol = &g.symbols[*symbol];
            if symbol == next_symbol {
                let mut item_copy = *item;
                item_copy.dot_index += 1;
                result.insert(item_copy);
            }
        }
    }
    closure(g, &result)
}
