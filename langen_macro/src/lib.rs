use proc_macro::TokenStream;
use regex_automata::dfa::dense;
use syn::{Data, DeriveInput, Ident, LitStr};

#[proc_macro_derive(Langen, attributes(token))]
pub fn langen_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    if let Data::Enum(data) = input.data {
        let name = input.ident;

        let mut token_idents = vec![];
        let mut token_patterns = vec![];

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
                token_idents.push(variant.ident);
                token_patterns.push(r.value());
            }
        }

        let dfa = dense::DFA::new_many(&token_patterns).expect("Couldn't build regex automaton");
        let (le_dfa_bytes, _) = dfa.to_bytes_little_endian();
        let (be_dfa_bytes, _) = dfa.to_bytes_big_endian();

        todo!()
    } else {
        panic!("Langen can only be used on enum");
    }
}
