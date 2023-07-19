use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display, hash::Hash,
};

use proc_macro2::Ident;

use crate::finite_automaton::*;

/// If ident is None and terminal is true, it's the eof token
/// If ident is None and terminal is false, it's epsilon
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
    pub first_map: HashMap<ParserSymbol, BTreeSet<ParserSymbol>>,
}

impl Grammar {
    pub fn compute_first(&mut self) {
        let mut result: HashMap<ParserSymbol, BTreeSet<ParserSymbol>> = HashMap::new();
        result.insert(ParserSymbol { ident: None, terminal: true }, BTreeSet::from([ParserSymbol { ident: None, terminal: true }]));
        let mut changed = false;
        while !changed {
            for symbol in &self.symbols {
                if symbol.terminal {
                    if result.insert(symbol.clone(), BTreeSet::from([symbol.clone()])) != Some(BTreeSet::from([symbol.clone()])) {
                        changed = true;
                    }
                } else {
                    let symbol_index = self.symbols.iter().position(|x| x==symbol).unwrap();
                    for rule in &self.rules {
                        if rule.0 == symbol_index {
                            if rule.1.is_empty() {
                                if let std::collections::hash_map::Entry::Vacant(_) =
                                result.entry(symbol.clone())
                                {
                                    result.insert(symbol.clone(), BTreeSet::from([ParserSymbol { ident: None, terminal: false }]));
                                    changed = true;
                                } else {
                                    changed |= result.get_mut(symbol).unwrap().insert(ParserSymbol { ident: None, terminal: false });
                                }
                            } else {
                                for index in &rule.1 {
                                    let part_symbol = &self.symbols[*index];
                                    let mut part = match result.get(part_symbol) {
                                        Some(x) => x.clone(),
                                        None => {break;},
                                    };
                                    let containes_epsilon = part.remove(&ParserSymbol { ident: None, terminal: false });
                                    if let std::collections::hash_map::Entry::Vacant(_) =
                                    result.entry(symbol.clone())
                                    {
                                        result.insert(symbol.clone(), part.clone());
                                        changed = true;
                                    } else {
                                        for element in part {
                                            changed |= result.get_mut(symbol).unwrap().insert(element.clone());
                                        }
                                    }
                                    if !containes_epsilon {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        self.first_map = result;
    }
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

#[derive(Debug, Clone, Ord, Eq, PartialOrd, PartialEq)]
struct Item {
    rule_index: usize,
    dot_index: usize,
    lookahead: BTreeSet<ParserSymbol>,
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
        print!("{:?}", g.symbols);
        println!(" MAP    {:#?}", g.first_map);
        check_start(&g);
        println!("Hey");
        let graph = generate_graph(g);
        println!("Boy");
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
            lookahead: BTreeSet::from([ParserSymbol { ident: None, terminal: true }]),
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
        for item in state {
            if item.rule_index == 0 && item.dot_index == g.rules[0].1.len() {
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
                continue;
            }
            if item.dot_index == g.rules[item.rule_index].1.len() {
                for symbol in &item.lookahead {
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
        result.insert(item.clone());
        stack.push(item.clone());
    }
    while !stack.is_empty() {
        let current = stack.pop().unwrap();
        if let Some(symbol) = g.rules[current.rule_index].1.get(current.dot_index) {
            let symbol = &g.symbols[*symbol];
            if !symbol.terminal {
                for (i, (rule, _)) in g.rules.iter().enumerate() {
                    if g.symbols.get(*rule).unwrap() == symbol {
                        let mut after = vec![];
                        for i in (current.dot_index+1)..(g.rules[current.rule_index].1.len()) {
                            after.push(g.symbols[g.rules[current.rule_index].1[i]].clone());
                        }
                        let mut lookahead = BTreeSet::new();
                        for l in &current.lookahead {
                            let mut copy = after.clone();
                            copy.push(l.clone());
                            lookahead.append(&mut first(g, &copy));
                        }
                        let item = Item {
                            rule_index: i,
                            dot_index: 0,
                            lookahead,
                        };
                        if !result.contains(&item) {
                            result.insert(item.clone());
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
                let mut item_copy = item.clone();
                item_copy.dot_index += 1;
                result.insert(item_copy);
            }
        }
    }
    closure(g, &result)
}

fn first(g: &Grammar, symbols: &Vec<ParserSymbol>) -> BTreeSet<ParserSymbol> {
    println!("Deep");
    let mut result = BTreeSet::new();
    for symbol in symbols {
        println!("{:?}", symbol);
        let mut part = g.first_map.get(symbol).unwrap().clone();
        let containes_epsilon = part.remove(&ParserSymbol { ident: None, terminal: false });
        result.append(&mut part);
        if !containes_epsilon {
            println!("Undeep1");
            return result;
        }
    }
    result.insert(ParserSymbol { ident: None, terminal: false });
    println!("Undeep2");
    result
}

