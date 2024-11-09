use std::{error::Error, fmt::Display};

pub use langen_macro::Tokens;
pub use regex_automata;

#[derive(Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

pub trait Tokens
where
    Self: Sized,
{
    fn scan(input: &str) -> Result<Vec<(Self, Span)>, LexerError>;
}

#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("No valid token at {0}")]
    NoToken(usize),
    #[error("Something went wrong during processing {1}: {0}")]
    ProcessError(Box<dyn Error>, Span),
}

#[derive(Debug)]
pub struct NoErr;

impl Display for NoErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "No error")
    }
}

impl Error for NoErr {}
