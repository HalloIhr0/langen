extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::ItemEnum;
use syn::{self, Attribute};

mod codegen;
mod finite_automaton;
mod lexer;
mod parser;

use crate::lexer::*;
use crate::parser::*;

#[proc_macro_derive(Langen, attributes(token, rule))]
pub fn langen_macro_fn(input: TokenStream) -> TokenStream {
    let base_enum: ItemEnum = syn::parse(input).expect("Langen must be applied to an enum");
    let name = base_enum.ident;

    let mut tokens = Vec::new();

    let mut symbols = Vec::new();
    let mut rules = Vec::new();

    for variant in &base_enum.variants {
        for attrib in &variant.attrs {
            match attrib.path.get_ident().unwrap().to_string().as_str() {
                "token" => {
                    let data = parse_token(attrib).unwrap_or_else(|| {
                        panic!(
                            "Invalid \"token\" argument for \"{}\", expected token(\"<regex>\"[, ignore=<bool>])",
                            variant.ident
                        )
                    });
                    tokens.push(TokenVariant {
                        name: variant.ident.clone(),
                        regex: data.0,
                        ignore: data.1,
                    });
                    if !symbols.contains(&Symbol {
                        ident: Some(variant.ident.clone()),
                        terminal: true,
                    }) {
                        symbols.push(Symbol {
                            ident: Some(variant.ident.clone()),
                            terminal: true,
                        });
                    }
                }
                "rule" => {
                    let idents = parse_rule(attrib).unwrap_or_else(|| {
                        panic!(
                            "Invalid \"rule\" argument for \"{}\", expected token(ident, ident, ...)",
                            variant.ident
                        )
                    });
                    if !symbols.contains(&Symbol {
                        ident: Some(variant.ident.clone()),
                        terminal: false,
                    }) {
                        symbols.push(Symbol {
                            ident: Some(variant.ident.clone()),
                            terminal: false,
                        });
                    }
                    rules.push((symbols.len() - 1, idents));
                }
                _ => continue,
            }
        }
    }

    let automaton = create_finite_automaton(tokens);

    let mut rules_indexed = Vec::new();
    for (r, l) in rules {
        let mut indexes = Vec::new();
        for ident in &l {
            let mut found = false;
            for (i, s) in symbols.iter().enumerate() {
                if Some(ident.clone()) == s.ident {
                    found = true;
                    indexes.push(i);
                    break;
                }
            }
            if !found {
                panic!(
                    "Symbol \"{}\" in rule for \"{}\" doesnt exist",
                    ident,
                    symbols[r].ident.as_ref().unwrap()
                );
            }
        }
        rules_indexed.push((r, indexes));
    }
    let mut grammar = Grammar {
        symbols,
        rules: rules_indexed,
    };

    let table = ParserTable::create(&mut grammar);
    println!("{:?}", table);

    let scan_code = codegen::generate_scan(automaton);
    let gen = quote! {
        impl #name {
            #scan_code
        }
    };
    gen.into()
}

fn parse_token(attrib: &Attribute) -> Option<(String, bool)> {
    match attrib.parse_meta() {
        Ok(syn::Meta::List(list)) => {
            let regex = match list.nested.first() {
                Some(syn::NestedMeta::Lit(syn::Lit::Str(lit))) => lit.value(),
                _ => {
                    return None;
                }
            };
            let mut ignore = false;
            for element in list.nested.iter() {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(option)) = element {
                    match option.path.get_ident().unwrap().to_string().as_str() {
                        "ignore" => match &option.lit {
                            syn::Lit::Bool(value) => ignore = value.value(),
                            _ => {
                                return None;
                            }
                        },
                        _ => {
                            return None;
                        }
                    }
                }
            }
            Some((regex, ignore))
        }
        _ => None,
    }
}

fn parse_rule(attrib: &Attribute) -> Option<Vec<Ident>> {
    match attrib.parse_meta() {
        Ok(syn::Meta::List(list)) => {
            let mut idents = Vec::new();
            for element in list.nested.iter() {
                if let syn::NestedMeta::Meta(syn::Meta::Path(path)) = element {
                    idents.push(path.get_ident().unwrap().clone())
                }
            }
            Some(idents)
        }
        _ => None,
    }
}
