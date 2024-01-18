extern crate proc_macro;

use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::quote;
use syn::Ident;
use syn::{self, Attribute};
use syn::{ExprClosure, ItemEnum};

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
    let mut symbol_fields = HashMap::new();

    for variant in &base_enum.variants {
        let fields = &variant.fields;
        symbol_fields.insert(variant.ident.clone(), fields.clone());
        for attrib in &variant.attrs {
            match attrib.path.get_ident().unwrap().to_string().as_str() {
                "token" => {
                    let (regex, ignore, fun) = parse_token(attrib).unwrap_or_else(|| {
                        panic!(
                            "Invalid \"token\" argument for \"{}\", expected token(\"<regex>\"[, ignore=<bool>][, ast_fun=Fn(String)->Result<(fields), &str>])",
                            variant.ident
                        )
                    });
                    tokens.push(TokenVariant {
                        name: variant.ident.clone(),
                        fields: fields.clone(),
                        regex,
                        ignore,
                        ast_fun: fun,
                    });
                    if !symbols.contains(&ParserSymbol::Terminal(variant.ident.clone())) {
                        symbols.push(ParserSymbol::Terminal(variant.ident.clone()));
                    }
                }
                "rule" => {
                    let (idents, fun) = parse_rule(attrib).unwrap_or_else(|| {
                        panic!(
                            "Invalid \"rule\" argument for \"{}\", expected token(ident, ident, ...[, ast_fun=Fn((ident1_params), (ident2_params), ...))->Result<(params), &str>])",
                            variant.ident
                        )
                    });
                    if !symbols.contains(&ParserSymbol::Symbol(variant.ident.clone())) {
                        symbols.push(ParserSymbol::Symbol(variant.ident.clone()));
                    }
                    rules.push((symbols.len() - 1, idents, fun));
                }
                _ => continue,
            }
        }
    }

    let automaton = create_finite_automaton(&tokens);

    let mut rules_indexed = Vec::new();
    for (r, l, fun) in rules {
        let mut indexes = Vec::new();
        for ident in &l {
            let mut found = false;
            for (i, s) in symbols.iter().enumerate() {
                if Some(ident.clone()) == s.get_ident() {
                    found = true;
                    indexes.push(i);
                    break;
                }
            }
            if !found {
                panic!(
                    "Symbol \"{}\" in rule for \"{}\" doesnt exist",
                    ident,
                    symbols[r].get_ident().as_ref().unwrap()
                );
            }
        }
        rules_indexed.push((r, indexes, fun));
    }
    let mut grammar = Grammar {
        symbols,
        rules: rules_indexed,
        first_map: HashMap::new(),
    };
    grammar.compute_first();

    let table = ParserTable::create(&mut grammar);
    // println!("{:#?}", table);

    let scan_code = codegen::generate_scan(automaton, &tokens);
    let parse_code = codegen::generate_parse(&grammar, &table, &symbol_fields);
    let gen = quote! {
        impl #name {
            #scan_code
            #parse_code
        }
    };
    gen.into()
}

fn parse_token(attrib: &Attribute) -> Option<(String, bool, Option<ExprClosure>)> {
    match attrib.parse_meta() {
        Ok(syn::Meta::List(list)) => {
            let regex = match list.nested.first() {
                Some(syn::NestedMeta::Lit(syn::Lit::Str(lit))) => lit.value(),
                _ => {
                    return None;
                }
            };
            let mut ignore = false;
            let mut fun = None;
            for element in list.nested.iter() {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(option)) = element {
                    match option.path.get_ident().unwrap().to_string().as_str() {
                        "ignore" => match &option.lit {
                            syn::Lit::Bool(value) => ignore = value.value(),
                            _ => {
                                return None;
                            }
                        },
                        "ast_fun" => match &option.lit {
                            // Having this as a string is a bit ugly, but works for now
                            syn::Lit::Str(value) => fun = Some(syn::parse_str::<ExprClosure>(&value.value()).unwrap_or_else(|e| {panic!("Expected closure, found {} (for toxen with regex \"{}\"\nError is: {}", value.value(), regex, e)})),
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
            Some((regex, ignore, fun))
        }
        _ => None,
    }
}

fn parse_rule(attrib: &Attribute) -> Option<(Vec<Ident>, ExprClosure)> {
    match attrib.parse_meta() {
        Ok(syn::Meta::List(list)) => {
            let mut idents = Vec::new();
            let mut fun = None;
            for element in list.nested.iter() {
                match element {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                        idents.push(path.get_ident().unwrap().clone())
                    }
                    syn::NestedMeta::Meta(syn::Meta::NameValue(option)) => {
                        if option.path.get_ident().unwrap().to_string().as_str() == "ast_fun" {
                            match &option.lit {
                                // Having this as a string is a bit ugly, but works for now
                                syn::Lit::Str(value) => {
                                    let closure = syn::parse_str::<ExprClosure>(&value.value()).unwrap_or_else(|e| {panic!("Expected closure, found {} (for rule with children \"{:?}\"\nError is: {}", value.value(), idents.iter().map(|e| e.to_string()).collect::<Vec<String>>(), e)});
                                    fun = Some(closure);
                                }
                                _ => {
                                    return None;
                                }
                            }
                        } else {
                            return None;
                        }
                    }
                    _ => {}
                }
            }
            Some((
                idents,
                fun.unwrap_or_else(|| panic!("Every rule needs to have an \"ast_fun\"")),
            ))
        }
        _ => None,
    }
}
