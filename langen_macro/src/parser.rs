use std::fmt::Display;

use proc_macro2::Ident;

#[derive(PartialEq)]
pub struct Symbol {
    pub ident: Ident,
    pub terminal: bool,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.ident.to_string(), self.terminal)
    }
}

pub struct Grammar {
    pub symbols: Vec<Symbol>,
    pub rules: Vec<(u32, Vec<u32>)>,
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i,s) in self.symbols.iter().enumerate() {
            writeln!(f, "{}: {}", i, s)?;
        }
        for r in &self.rules {
            write!(f, "{} =>", r.0)?;
            for s in &r.1 {
                write!(f, " {}", s)?;
            }
            write!(f, "\n")?;
        }

        Ok(())
    }
}