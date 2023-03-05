use std::{collections::BTreeSet, fmt::Display};

use proc_macro2::Ident;

/// If ident is None, parsing is finished
#[derive(PartialEq)]
pub struct Symbol {
    pub ident: Option<Ident>,
    pub terminal: bool,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ident {
            Some(value) => write!(f, "{} ({})", value, self.terminal),
            None => write!(f, "START STATE ({})", self.terminal),
        }
    }
}

pub struct Grammar {
    pub symbols: Vec<Symbol>,
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

pub fn create_parser_table(g: &mut Grammar) {
    create_new_start(g);
    println!("{g}");
}

/// Adds a new symbol to the end of the list, which just links to the first nonterminal symbol
fn create_new_start(g: &mut Grammar) {
    let mut first = None;
    for (i, symbol) in g.symbols.iter().enumerate() {
        if !symbol.terminal {
            first = Some(i);
            break;
        }
    }
    if first.is_none() {
        panic!("No rules defined")
    }

    g.symbols.push(Symbol {
        ident: None,
        terminal: false,
    });
    g.rules
        .push((g.symbols.len() - 1, Vec::from([first.unwrap()])));
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

fn goto(g: &Grammar, items: &BTreeSet<Item>, next_symbol: &Symbol) -> BTreeSet<Item> {
    let mut result = BTreeSet::new();
    let mut stack = Vec::new();
    for item in items {
        stack.push(*item);
    }
    while !stack.is_empty() {
        let mut current = stack.pop().unwrap();
        if let Some(symbol) = g.rules[current.rule_index].1.get(current.dot_index) {
            let symbol = &g.symbols[*symbol];
            if symbol == next_symbol {
                current.dot_index += 1;
                for item in closure(g, &BTreeSet::from([current])) {
                    if !result.contains(&item) {
                        result.insert(item);
                        stack.push(item);
                    }
                }
            }
        }
    }
    result
}
