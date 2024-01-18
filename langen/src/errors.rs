use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Incomplete token at end of input")]
    IncompleteToken,
    #[error("Invalid character at index {0}")]
    InvalidChar(usize),
    #[error("Error during Lexer: {0}")]
    AstError(String),
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected end of tokens")]
    UnexpectedEnd,
    #[error("Invalid symbol at index {0}")]
    InvalidSymbol(usize),
    #[error("Error during Parser: {0}")]
    AstError(String),
}
