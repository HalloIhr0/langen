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

pub trait Tokens
where
    Self: Sized,
{
    fn scan(input: &str) -> Result<Vec<(Self, Span)>, LexerError> {
        Self::scan_bytes(input.as_bytes())
    }
    fn scan_bytes(input: &[u8]) -> Result<Vec<(Self, Span)>, LexerError>;
}

#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("No valid token at {0}")]
    NoToken(usize),
}
