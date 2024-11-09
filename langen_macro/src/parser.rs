use std::{collections::HashSet, hash::Hash, ptr, rc::Rc};

#[derive(PartialEq, Clone, Debug)]
pub enum MetaSymbol {
    Normal(usize),
    Start,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Symbol {
    Terminal(usize),
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
}

impl Element {
    fn next_symbol(&self) -> Option<&Symbol> {
        self.rule.parts.get(self.pos)
    }

    fn new_dot_advanced(&self) -> Self {
        Self {
            rule: self.rule.clone(),
            pos: self.pos + 1,
        }
    }
}

impl PartialEq for Element {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rule, &other.rule) && self.pos == other.pos
    }
}

impl Eq for Element {}

impl Hash for Element {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // https://users.rust-lang.org/t/hash-based-on-address-not-value-of-rc/28824
        ptr::hash(&*self.rule, state);
        self.pos.hash(state);
    }
}

#[derive(Debug)]
struct Transition {
    from: usize,
    to: usize,
    symbol: Symbol,
}

// Algorithms from https://amor.cms.hu-berlin.de/~kunert/papers/lr-analyse/lr.pdf
// For now this is LR(0)
#[derive(Debug)]
pub struct Lr1Automaton {
    rules: Vec<Rc<Rule>>,
    states: Vec<HashSet<Element>>,
    transitions: Vec<Transition>,
}

impl Lr1Automaton {
    pub fn create(rules: Vec<Rule>) -> Self {
        Self {
            rules: rules.into_iter().map(|rule| Rc::new(rule)).collect(),
            states: vec![],
            transitions: vec![],
        }
    }

    // Being able to use self is cleaner
    pub fn build_automaton(&mut self) {
        self.rules.push(Rc::new(Rule {
            parts: vec![Symbol::NonTerminal(MetaSymbol::Normal(0))],
            result: MetaSymbol::Start,
        }));

        let start = self.closure(&HashSet::from([Element {
            rule: self
                .rules
                .last()
                .expect("Has been inserted two lines above")
                .clone(),
            pos: 0,
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
                        };
                        if !result.contains(&new_elem) {
                            queue.push(new_elem);
                        }
                    }
                }
            }

            result.insert(elem);
        }
        result
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
}
