use lexer_automaton::Automaton;
use proc_macro::TokenStream;
use regex_syntax::hir::Hir;
use syn::{Data, DeriveInput, Ident, LitStr};

mod lexer_automaton;

#[derive(Debug)]
struct Token {
    ident: Ident,
    regex: Hir,
}

#[proc_macro_derive(Langen, attributes(token))]
pub fn langen_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    if let Data::Enum(data) = input.data {
        let name = input.ident;
        let regex_parser = regex_syntax::ParserBuilder::default();

        let mut tokens = vec![];

        for variant in data.variants {
            if let Some(r) = variant.attrs.iter().find_map(|attr| {
                if attr.path().is_ident("token") {
                    let t: LitStr = attr.parse_args().unwrap_or_else(|_| {
                        panic!(
                            "token argument for \"{}\" must be string literal",
                            variant.ident
                        )
                    });
                    Some(t)
                } else {
                    None
                }
            }) {
                let regex = regex_parser.build().parse(&r.value()).unwrap_or_else(|_| {
                    panic!(
                        "Invalid regex \"{}\" in token \"{}\"",
                        r.value(),
                        variant.ident
                    )
                });
                tokens.push(Token {
                    ident: variant.ident,
                    regex,
                });
            }
        }

        let automaton = Automaton::from_tokens(&tokens);
        let automaton = automaton.to_dfa();
        panic!("{}", automaton.to_graphvis());
    } else {
        panic!("Langen can only be used on enum");
    }
}
