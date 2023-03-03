# Langen
A tool to create programming languages

**Please install `langen` instead of `langen_macro` for this tool to work correctly!**

Most of the work is done during compilation, so it should run quite fast
## Usage
```rust
use langen;

#[derive(Debug, PartialEq)]
#[derive(langen::Langen)]
enum Tokens {
    #[token(r"[ \t\r]", ignore=true)]
    _Ignore,

    #[token(r"let")]
    Let,

    #[token(r"=")]
    Assign,

    #[token(r";")]
    Semicolon,

    #[token(r"[0-9]+")]
    #[token(r"0x[0-9A-F]+")]
    IntLiteral,

    #[token(r"[A-Za-z_]+")]
    Identifier,
}

fn main() {
    let tokens = Tokens::scan("let variable = 312;").unwrap();
    let mut iter = tokens.iter();
    assert_eq!(iter.next(), Some(&Tokens::Let));
    assert_eq!(iter.next(), Some(&Tokens::Identifier));
    assert_eq!(iter.next(), Some(&Tokens::Assign));
    assert_eq!(iter.next(), Some(&Tokens::IntLiteral));
    assert_eq!(iter.next(), Some(&Tokens::Semicolon));
    assert_eq!(iter.next(), None);
}
```
To use langen, derive `langen::Langen` on an enum. To define a token, add `#[token()]` to it. The first argument inside token will always be a regex. The tokens defined first will get priority (for example `Let` has a higher priority than `Identifier`, although they match the same input). You can also define multiple tokens for one enum variant, for them all to produce that variant. You can optionally add `ignore=true`, so that the token doesn't add anything to the output. You can get the parsed tokens by calling `scan(input)` on the enum.
## Features
- Lexer
## Planned features
- Parser
- AST generation
## Licence
This project is licenced under the MIT licence
