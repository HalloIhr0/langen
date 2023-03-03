use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Incomplete token at end of input")]
    IncompleteToken,
    #[error("Invalid character at index {0}")]
    InvalidChar(usize),
}
