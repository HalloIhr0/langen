# Langen
A tool to create programming languages

**Please depend on `langen` instead of `langen_macro` for this to work correctly!**

The crate provides a set of derive macros to generate a lexer and parser. All the heavy computations are done during compilation.

Look at the docs for more information on how to use this.

## Usage
``` rust
use langen::{Grammar, NoErr, Span, Tokens};

#[derive(Tokens, Grammar, Debug)]
#[ignored(r"[ \t]")]
enum Math {
    #[token(r"\(")]
    LParen,
    #[token(r"\)")]
    RParen,
    #[token(r"\+")]
    Plus,
    #[token(r"-")]
    Minus,
    #[token(r"\*")]
    Mul,
    #[token(r"/")]
    Div,
    #[token(r"[0-9]+", |s: &str| {s.parse()})]
    IntLit(i32),
    #[rule(|_span: Span, expr: i32, term: i32| Ok::<i32, NoErr>(expr+term), Expression, Plus, Term)]
    #[rule(|_span: Span, expr: i32, term: i32| Ok::<i32, NoErr>(expr+term), Expression, Minus, Term)]
    #[rule(|_span: Span, term: i32| Ok::<i32, NoErr>(term), Term)]
    #[rule(|_span: Span, term: i32| Ok::<i32, NoErr>(-term), Minus, Factor)]
    Expression(i32),
    #[rule(|_span: Span, term: i32, factor: i32| Ok::<i32, NoErr>(term*factor), Term, Mul, Factor)]
    #[rule(|_span: Span, term: i32, factor: i32| Ok::<i32, NoErr>(term/factor), Term, Div, Factor)]
    #[rule(|_span: Span, factor: i32| Ok::<i32, NoErr>(factor), Factor)]
    Term(i32),
    #[rule(|_span: Span, intlit: i32| Ok::<i32, NoErr>(intlit), IntLit)]
    #[rule(|_span: Span, expr: i32| Ok::<i32, NoErr>(expr), LParen, Expression, RParen)]
    Factor(i32),
}

let input = "(33+5) * (-34)";
let tokens = Math::scan(input).unwrap();
for (t, s) in &tokens {
    println!("{}\t{:?}", s, t);
}
let result = Math::parse(tokens).unwrap();
println!("{:?}", result);
```

## License
This project is licensed under the MIT license
