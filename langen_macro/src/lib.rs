extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::ItemEnum;
use syn::{self, Attribute};

mod codegen;
mod finite_automaton;
mod lexer;

use crate::lexer::*;

#[proc_macro_derive(Langen, attributes(token))]
pub fn langen_macro_fn(input: TokenStream) -> TokenStream {
    let base_enum: ItemEnum = syn::parse(input).expect("Langen must be applied to an enum");
    let name = base_enum.ident;

    let mut tokens = Vec::new();

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
                    })
                }
                _ => continue,
            }
        }
    }

    let automaton = create_finite_automaton(tokens);
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
