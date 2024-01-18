use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
    hash::Hash,
};

use syn::ExprClosure;
use syn::Ident;

use crate::finite_automaton::*;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum ParserSymbol {
    Symbol(Ident),
    Terminal(Ident),
    Eof,
    Epsilon,
    Start,
}

impl ParserSymbol {
    pub fn get_ident(&self) -> Option<Ident> {
        match self {
            ParserSymbol::Symbol(ident) => Some(ident.clone()),
            ParserSymbol::Terminal(ident) => Some(ident.clone()),
            ParserSymbol::Eof => None,
            ParserSymbol::Epsilon => None,
            ParserSymbol::Start => None,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            ParserSymbol::Symbol(..) => false,
            ParserSymbol::Terminal(..) => true,
            ParserSymbol::Eof => true,
            ParserSymbol::Epsilon => todo!(),
            ParserSymbol::Start => false,
        }
    }
}

impl Display for ParserSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserSymbol::Symbol(ident) => write!(f, "{}", ident),
            ParserSymbol::Terminal(ident) => write!(f, "{} (TERMINAL)", ident),
            ParserSymbol::Eof => write!(f, "EOF"),
            ParserSymbol::Epsilon => write!(f, "EPSILON"),
            ParserSymbol::Start => write!(f, "START"),
        }
    }
}

pub struct Grammar {
    pub symbols: Vec<ParserSymbol>,
    pub rules: Vec<(usize, Vec<usize>, ExprClosure)>,
    pub first_map: HashMap<ParserSymbol, BTreeSet<ParserSymbol>>,
}

impl Grammar {
    pub fn compute_first(&mut self) {
        self.first_map
            .entry(ParserSymbol::Eof)
            .or_insert_with(BTreeSet::new)
            .insert(ParserSymbol::Eof);
        for symbol in &self.symbols {
            if symbol.is_terminal() {
                self.first_map
                    .entry(symbol.clone())
                    .or_insert_with(BTreeSet::new)
                    .insert(symbol.clone());
            }
        }
        for (from, to, _) in &self.rules {
            if to.is_empty() {
                // Epsilon
                self.first_map
                    .entry(self.symbols[*from].clone())
                    .or_insert_with(BTreeSet::new)
                    .insert(ParserSymbol::Epsilon);
            }
        }
        let mut changed = true;
        while changed {
            changed = false;
            for (from, to, _) in &self.rules {
                let mut broken = false; // This name is... interesting?... It's better than "breaked" tho
                for symbol in to {
                    let symbol = &self.symbols[*symbol];
                    let mut first = self
                        .first_map
                        .get(symbol)
                        .unwrap_or(&BTreeSet::new())
                        .clone();
                    let has_epsilon = first.remove(&ParserSymbol::Epsilon);
                    let entry = self
                        .first_map
                        .entry(self.symbols[*from].clone())
                        .or_insert_with(BTreeSet::new);
                    let prev = entry.clone();
                    entry.append(&mut first);
                    if prev != *entry {
                        changed = true;
                    }
                    if !has_epsilon {
                        broken = true;
                        break;
                    }
                }
                if !broken {
                    let entry = self
                        .first_map
                        .entry(self.symbols[*from].clone())
                        .or_insert_with(BTreeSet::new);
                    let prev = entry.clone();
                    entry.insert(ParserSymbol::Epsilon);
                    if prev != *entry {
                        changed = true;
                    }
                }
            }
        }
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
        check_start(g);
        let graph = generate_graph(g);
        // println!("{graph}");
        create_table(g, &graph)
    }
}

fn check_start(g: &Grammar) {
    let start_symbol = g.rules[0].0;
    let mut found_start = false;
    for (from, to, _) in &g.rules {
        if to.contains(&start_symbol) {
            panic!("Rule for \"{}\" can't produce the start symbol\nConsider adding a new start symbol which hast a rule that converts it to the original start symbol", g.symbols[*from].get_ident().as_ref().unwrap());
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
            lookahead: BTreeSet::from([ParserSymbol::Eof]),
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
        match transition.transition.is_terminal() {
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
                action_table.insert((i, ParserSymbol::Eof), Action::Accept);
                continue;
            }
            if item.dot_index == g.rules[item.rule_index].1.len() {
                for symbol in &item.lookahead {
                    if symbol.is_terminal() {
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
    while let Some(current) = stack.pop() {
        if let Some(symbol) = g.rules[current.rule_index].1.get(current.dot_index) {
            let next_symbol = &g.symbols[*symbol];
            if !next_symbol.is_terminal() {
                for (rule_index, (rule_from, _, _)) in g.rules.iter().enumerate() {
                    if g.symbols.get(*rule_from).unwrap() == next_symbol {
                        let mut after = vec![];
                        for i in (current.dot_index + 1)..(g.rules[current.rule_index].1.len()) {
                            after.push(g.symbols[g.rules[current.rule_index].1[i]].clone());
                        }
                        let mut lookahead = BTreeSet::new();
                        for l in &current.lookahead {
                            let mut new = after.clone();
                            new.push(l.clone());
                            lookahead.append(&mut first(g, &new));
                        }
                        let item = Item {
                            rule_index,
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
    // Merge simmilar states
    let mut merged_results: Vec<Item> = Vec::new(); // cant iter mut over a BTreeSet (i think)
    'item_loop: for mut item in result {
        for already_merged in merged_results.iter_mut() {
            if item.rule_index == already_merged.rule_index
                && item.dot_index == already_merged.dot_index
            {
                // only lookahead is different
                already_merged.lookahead.append(&mut item.lookahead);
                continue 'item_loop;
            }
        }
        merged_results.push(item);
    }
    BTreeSet::from_iter(merged_results.iter().cloned())
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
    let mut result = BTreeSet::new();
    for symbol in symbols {
        let mut first = g.first_map.get(symbol).unwrap_or(&BTreeSet::new()).clone();
        let has_epsilon = first.remove(&ParserSymbol::Epsilon);
        result.append(&mut first);
        if !has_epsilon {
            return result;
        }
    }

    result.insert(ParserSymbol::Epsilon);

    result
}
