//! # Langen - A tool to create programming languages
//!
//! ``` rust
//! use langen::{Grammar, NoErr, Span, Tokens};
//!
//! #[derive(Tokens, Grammar, Debug)]
//! #[ignored(r"[ \t]")]
//! enum Math {
//!     #[token(r"\(")]
//!     LParen,
//!
//!     #[token(r"\)")]
//!     RParen,
//!
//!     #[token(r"\+")]
//!     Plus,
//!
//!     #[token(r"-")]
//!     Minus,
//!
//!     #[token(r"\*")]
//!     Mul,
//!
//!     #[token(r"/")]
//!     Div,
//!
//!     #[token(r"[0-9]+", |s: &str| {s.parse()})]
//!     IntLit(i32),
//!
//!     #[rule(|_span: Span, expr: i32, term: i32| Ok::<i32, NoErr>(expr+term), Expression, Plus, Term)]
//!     #[rule(|_span: Span, expr: i32, term: i32| Ok::<i32, NoErr>(expr+term), Expression, Minus, Term)]
//!     #[rule(|_span: Span, term: i32| Ok::<i32, NoErr>(term), Term)]
//!     #[rule(|_span: Span, term: i32| Ok::<i32, NoErr>(-term), Minus, Factor)]
//!     Expression(i32),
//!
//!     #[rule(|_span: Span, term: i32, factor: i32| Ok::<i32, NoErr>(term*factor), Term, Mul, Factor)]
//!     #[rule(|_span: Span, term: i32, factor: i32| Ok::<i32, NoErr>(term/factor), Term, Div, Factor)]
//!     #[rule(|_span: Span, factor: i32| Ok::<i32, NoErr>(factor), Factor)]
//!     Term(i32),
//!
//!     #[rule(|_span: Span, intlit: i32| Ok::<i32, NoErr>(intlit), IntLit)]
//!     #[rule(|_span: Span, expr: i32| Ok::<i32, NoErr>(expr), LParen, Expression, RParen)]
//!     Factor(i32),
//! }
//!
//! let input = "(33+5) * (-34)";
//! let tokens = Math::scan(input).unwrap();
//! for (t, s) in &tokens {
//!     println!("{}\t{:?}", s, t);
//! }
//! let result = Math::parse(tokens).unwrap();
//! println!("{:?}", result);
//! ```

use std::{
    error::Error,
    fmt::{Debug, Display},
};

pub use langen_macro::{Grammar, Tokens};
pub use regex_automata;

/// A range of characters in the input file
#[derive(Clone, Debug)]
pub struct Span {
    /// The start of the range (inclusive)
    pub start: usize,
    /// The end of the range (exclusive)
    pub end: usize,
}

impl Span {
    /// Creates a new span
    #[must_use]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// Use the Tokens derive macro to generate a lexer. See [`derive@Tokens`] for more info.
pub trait Tokens
where
    Self: Sized,
{
    /// Converts a input string into a sequence of tokens
    ///
    /// # Errors
    /// Return an error if something went wrong during the lexical analysis. See [`LexerError`] for more info.
    fn scan(input: &str) -> Result<Vec<(Self, Span)>, LexerError>;
}

/// Use the Grammar derive macro to generate a parser. See [`derive@Grammar`] for more info.
pub trait Grammar
where
    Self: Sized + Debug,
{
    /// The data type of the first symbol
    type OUT;

    /// Check the grammar of a sequence of tokens and compute a value from them
    ///
    /// # Errors
    /// Return an error if something went wrong during the syntactical analysis. See [`ParserError`] for more info.
    fn parse(tokens: Vec<(Self, Span)>) -> Result<Self::OUT, ParserError<Self>>;
}

/// Possible runtime errors during the lexical analysis
#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    /// No valid token could be read at the specified position
    #[error("No valid token at {0}")]
    NoToken(usize),
    /// A `process_func` returned an `Err()`
    #[error("Something went wrong during processing {1}: {0}")]
    ProcessError(Box<dyn Error>, Span),
}

/// Possible runtime errors during the syntactical analysis
#[derive(thiserror::Error, Debug)]
pub enum ParserError<T: Debug> {
    /// The parser unexpectedly reached the end of the input
    #[error("Unexpected ending")]
    UnexpectedEnd,
    #[error("Invalid Token {0} ({2}): {1:?}")]
    /// The parser encountered an invalid token at the position specified
    InvalidToken(usize, T, Span),
    #[error("Something went wrong during processing {0} ({2}): {1}")]
    /// A `process_func` returned an `Err()`
    ProcessError(usize, Box<dyn Error>, Span),
}

/// A placeholder type that you can use in the type of `process_func`s that can't error
#[derive(Debug)]
pub struct NoErr;

impl Display for NoErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "No error")
    }
}

impl Error for NoErr {}
