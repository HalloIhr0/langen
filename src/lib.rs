extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use regex_syntax::Parser;
use syn::ItemEnum;
use syn::{self, Attribute};

use crate::finite_automaton::FiniteAutomaton;

mod finite_automaton;

#[proc_macro_derive(Langen, attributes(token))]
pub fn langen_macro_fn(input: TokenStream) -> TokenStream {
    let base_enum: ItemEnum = syn::parse(input).expect("Langen must be applied to an enum");
    let name = base_enum.ident;

    for variant in &base_enum.variants {
        for attrib in &variant.attrs {
            match attrib.path.get_ident().unwrap().to_string().as_str() {
                "token" => {
                    let regex = parse_token(attrib).unwrap_or_else(|| {
                        panic!(
                            "Invalid \"token\" argument for \"{}\", expected #[token(\"<regex>\")]",
                            variant.ident
                        )
                    });
                    let nfa = FiniteAutomaton::from_regex(
                        &Parser::new().parse(&regex).unwrap_or_else(|_| {
                            panic!(
                                "Invalid invalid regex \"{}\" for token \"{}\"",
                                regex, variant.ident
                            )
                        }),
                    )
                    .unwrap_or_else(|| {
                        panic!(
                            "Couldnt parse regex \"{}\" for token \"{}\"",
                            regex, variant.ident
                        )
                    });
                    println!("{}\n", nfa);
                }
                _ => continue,
            }
        }
    }

    let gen = quote! {
        impl #name {
            fn test() {
                println!("Hello World!");
            }
        }
    };
    gen.into()
}

fn parse_token(attrib: &Attribute) -> Option<String> {
    match attrib.parse_meta() {
        Ok(syn::Meta::List(list)) => match list.nested.first() {
            Some(syn::NestedMeta::Lit(syn::Lit::Str(lit))) => Some(lit.value()),
            _ => None,
        },
        _ => None,
    }
}
