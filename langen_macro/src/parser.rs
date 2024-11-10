use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ptr,
    rc::Rc,
};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Terminal {
    Normal(usize),
    Eof,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum MetaSymbol {
    Normal(usize),
    Start,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Symbol {
    Terminal(Terminal),
    NonTerminal(MetaSymbol),
}

#[derive(Debug)]
pub struct Rule {
    pub parts: Vec<Symbol>,
    pub result: MetaSymbol,
}

#[derive(Clone, Debug)]
struct Element {
    rule: Rc<Rule>,
    pos: usize,
    lookahead: HashSet<Terminal>,
}

impl Element {
    fn next_symbol(&self) -> Option<&Symbol> {
        self.rule.parts.get(self.pos)
    }

    fn new_dot_advanced(&self) -> Self {
        Self {
            rule: self.rule.clone(),
            pos: self.pos + 1,
            lookahead: self.lookahead.clone(),
        }
    }
}

impl PartialEq for Element {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rule, &other.rule)
            && self.pos == other.pos
            && self.lookahead == other.lookahead
    }
}

impl Eq for Element {}

impl Hash for Element {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // https://users.rust-lang.org/t/hash-based-on-address-not-value-of-rc/28824
        ptr::hash(&*self.rule, state);
        self.pos.hash(state);
        // We don't hash lookahead
        // But since the condition is just "k1 == k2 -> hash(k1) == hash(k2)", this should still be fine
        // We might just have a few hash collisions, but compile time performance doesn't matter anyways
    }
}

#[derive(Debug)]
struct Transition {
    from: usize,
    to: usize,
    symbol: Symbol,
}

#[derive(Clone, Debug)]
struct FirstSet {
    set: HashSet<Terminal>,
    contains_epsilon: bool,
}

// Algorithms from https://amor.cms.hu-berlin.de/~kunert/papers/lr-analyse/lr.pdf
#[derive(Debug)]
pub struct Lr1Automaton {
    rules: Vec<Rc<Rule>>,
    states: Vec<HashSet<Element>>,
    transitions: Vec<Transition>,
    first_sets: HashMap<Symbol, FirstSet>,
    num_terminal: usize,
    num_nonterminal: usize,
}

#[derive(Debug)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Accept
}

impl Lr1Automaton {
    pub fn create(rules: Vec<Rc<Rule>>, num_terminal: usize, num_nonterminal: usize) -> Self {
        Self {
            rules,
            states: vec![],
            transitions: vec![],
            first_sets: HashMap::new(),
            num_terminal,
            num_nonterminal,
        }
    }

    // Being able to use self is cleaner
    pub fn build_automaton(&mut self) {
        self.rules.push(Rc::new(Rule {
            parts: vec![Symbol::NonTerminal(MetaSymbol::Normal(0))],
            result: MetaSymbol::Start,
        }));

        self.build_first_sets();

        let start = self.closure(&HashSet::from([Element {
            rule: self
                .rules
                .last()
                .expect("Has been inserted two lines above")
                .clone(),
            pos: 0,
            lookahead: HashSet::from([Terminal::Eof]),
        }]));
        self.states.push(start);

        let mut i = 0;
        while i < self.states.len() {
            let mut possible_next_symbols = vec![];
            for elem in &self.states[i] {
                if let Some(symbol) = elem.next_symbol() {
                    if !possible_next_symbols.contains(symbol) {
                        possible_next_symbols.push(symbol.clone());
                    }
                }
            }
            for symbol in possible_next_symbols {
                let new = self.goto(&self.states[i], &symbol);
                if let Some(new_i) = self.states.iter().position(|state| *state == new) {
                    self.transitions.push(Transition {
                        from: i,
                        to: new_i,
                        symbol,
                    });
                } else {
                    self.states.push(new);
                    self.transitions.push(Transition {
                        from: i,
                        to: self.states.len() - 1,
                        symbol,
                    });
                }
            }
            i += 1;
        }
    }

    /// Returns (action, jump)
    /// 
    /// action is state num primary, symbol secondary
    /// 
    /// jump is metasymbol num primary, state secondary
    pub fn generate_tables(&self) -> (Vec<HashMap<Terminal, Action>>, Vec<HashMap<usize, usize>>) {
        let mut action = vec![];
        let mut jump = vec![HashMap::new(); self.num_nonterminal];
        for (i, state) in self.states.iter().enumerate() {
            let mut action_row = HashMap::new();

            for transition in &self.transitions {
                if transition.from == i {
                    match &transition.symbol {
                        Symbol::Terminal(symbol) => {
                            if action_row.insert(symbol.clone(), Action::Shift(transition.to)).is_some() {
                                eprintln!("######## Conflict ########");
                            }
                        }
                        Symbol::NonTerminal(MetaSymbol::Normal(symbol)) => {
                            if jump[*symbol].insert(i, transition.to).is_some() {
                                eprintln!("######## Conflict ########");
                            }
                        }
                        _ => {unreachable!("The automaton shouldn't be able to contain these as transitions")}
                    }
                }
            }

            for element in state {
                if element.pos == element.rule.parts.len() {
                    for symbol in &element.lookahead {
                        if element.rule.result == MetaSymbol::Start && *symbol == Terminal::Eof {
                            if action_row.insert(Terminal::Eof, Action::Accept).is_some() {
                                eprintln!("######## Conflict ########");
                            }
                        } else {
                            let rule_i = self.rules.iter().position(|rule| Rc::ptr_eq(rule, &element.rule)).expect("Must contain rule");
                            if action_row.insert(symbol.clone(), Action::Reduce(rule_i)).is_some() {
                                eprintln!("######## Conflict ########");
                            }
                        }
                    }
                }
            }

            action.push(action_row);
        }
        (action, jump)
    }

    fn build_first_sets(&mut self) {
        self.first_sets.insert(
            Symbol::Terminal(Terminal::Eof),
            FirstSet {
                set: HashSet::from([Terminal::Eof]),
                contains_epsilon: false,
            },
        );
        for i in 0..self.num_terminal {
            self.first_sets.insert(
                Symbol::Terminal(Terminal::Normal(i)),
                FirstSet {
                    set: HashSet::from([Terminal::Normal(i)]),
                    contains_epsilon: false,
                },
            );
        }
        self.first_sets.insert(
            Symbol::NonTerminal(MetaSymbol::Start),
            FirstSet {
                set: HashSet::new(),
                contains_epsilon: false,
            },
        );
        for i in 0..self.num_nonterminal {
            self.first_sets.insert(
                Symbol::NonTerminal(MetaSymbol::Normal(i)),
                FirstSet {
                    set: HashSet::new(),
                    contains_epsilon: false,
                },
            );
        }

        let mut changed = true;
        while changed {
            changed = false;
            'outer: for rule in &self.rules {
                for part in &rule.parts {
                    let part_first_set = self
                        .first_sets
                        .get(part)
                        .expect("Has been inserted above")
                        .clone();
                    for symbol in part_first_set.set {
                        changed |= self
                            .first_sets
                            .get_mut(&Symbol::NonTerminal(rule.result.clone()))
                            .expect("Has been inserted above")
                            .set
                            .insert(symbol);
                    }
                    if !part_first_set.contains_epsilon {
                        continue 'outer;
                    }
                }
                if !self
                    .first_sets
                    .get(&Symbol::NonTerminal(rule.result.clone()))
                    .expect("Has been inserted above")
                    .contains_epsilon
                {
                    self.first_sets
                        .get_mut(&Symbol::NonTerminal(rule.result.clone()))
                        .expect("Has been inserted above")
                        .contains_epsilon = true;
                    changed = true;
                }
            }
        }
    }

    fn closure(&self, elems: &HashSet<Element>) -> HashSet<Element> {
        let mut result = HashSet::new();
        let mut queue = elems.iter().cloned().collect::<Vec<_>>();

        while let Some(elem) = queue.pop() {
            if let Some(Symbol::NonTerminal(symbol)) = elem.next_symbol() {
                for rule in &self.rules {
                    if rule.result == *symbol {
                        let new_elem = Element {
                            rule: rule.clone(),
                            pos: 0,
                            lookahead: self
                                .first(&elem.rule.parts[(elem.pos + 1)..], &elem.lookahead),
                        };
                        if !result.contains(&new_elem) {
                            queue.push(new_elem);
                        }
                    }
                }
            }

            result.insert(elem);
        }

        // combine elements that only differ in lookahead
        let mut combined_result: HashSet<Element> = HashSet::new();
        for elem in result {
            if let Some(mut existing_elem) = combined_result
                .iter()
                .find(|existing_elem| {
                    Rc::ptr_eq(&existing_elem.rule, &elem.rule) && existing_elem.pos == elem.pos
                })
                .cloned()
            {
                combined_result.remove(&existing_elem);
                existing_elem.lookahead = existing_elem
                    .lookahead
                    .union(&elem.lookahead)
                    .cloned()
                    .collect();
                combined_result.insert(existing_elem);
            } else {
                combined_result.insert(elem);
            }
        }
        combined_result
    }

    fn goto(&self, elems: &HashSet<Element>, symbol: &Symbol) -> HashSet<Element> {
        let mut result = HashSet::new();
        for elem in elems {
            if elem.next_symbol().is_some_and(|s| s == symbol) {
                result.insert(elem.new_dot_advanced());
            }
        }
        self.closure(&result)
    }

    // This result can never contain epsilon (because set is just terminals), so just returning a HashSet<Terminal> is enough
    fn first(&self, prefix: &[Symbol], set: &HashSet<Terminal>) -> HashSet<Terminal> {
        let mut result = HashSet::new();

        for symbol in prefix {
            let first_set = self.first_sets.get(symbol).expect("Should always exist");
            result = result.union(&first_set.set).cloned().collect();
            if !first_set.contains_epsilon {
                // Since we always end in prefix, there is no need to check the set symbols
                return result;
            }
        }

        // since set is just terminals and first(terminal)={terminal}, this is valid
        result.union(set).cloned().collect()
    }
}

impl Display for Lr1Automaton {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, state) in self.states.iter().enumerate() {
            writeln!(f, "{i}:")?;
            for element in state {
                writeln!(
                    f,
                    "    {:?} @ {} ({:?})",
                    element.rule, element.pos, element.lookahead
                )?;
            }
        }

        for transition in &self.transitions {
            writeln!(
                f,
                "{} -> {} ({:?})",
                transition.from, transition.to, transition.symbol
            )?;
        }

        Ok(())
    }
}
