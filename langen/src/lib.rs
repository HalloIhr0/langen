use std::{error::Error, fmt::{Debug, Display}};

pub use langen_macro::{Grammar, Tokens};
pub use regex_automata;

#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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

pub trait Grammar
where
    Self: Sized + Debug,
{
    type OUT;
    fn parse(tokens: Vec<(Self, Span)>) -> Result<Self::OUT, ParserError<Self>>;
}

#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("No valid token at {0}")]
    NoToken(usize),
    #[error("Something went wrong during processing {1}: {0}")]
    ProcessError(Box<dyn Error>, Span),
}

#[derive(thiserror::Error, Debug)]
pub enum ParserError<T: Debug> {
    #[error("Unexpected ending")]
    UnexpectedEnd,
    #[error("Invalid Token {0} ({2}): {1:?}")]
    InvalidToken(usize, T, Span),
    #[error("Something went wrong during processing {0} ({2}): {1}")]
    ProcessError(usize, Box<dyn Error>, Span),
}

#[derive(Debug)]
pub struct NoErr;

impl Display for NoErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "No error")
    }
}

impl Error for NoErr {}
