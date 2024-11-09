use proc_macro::TokenStream;
use quote::quote;
use regex_automata::dfa::dense;
use syn::{Data, DeriveInput, Ident, LitStr};

#[proc_macro_derive(Tokens, attributes(token))]
pub fn langen_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    if let Data::Enum(data) = input.data {
        let name = input.ident;

        let mut token_indices = vec![];
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
                token_indices.push(token_idents.len());
                token_idents.push(variant.ident);
                token_patterns.push(r.value());
            }
        }

        let dfa = dense::DFA::new_many(&token_patterns).expect("Couldn't build regex automaton");
        let (bytes, pad) = dfa.to_bytes_little_endian();
        let le_dfa_bytes = &bytes[pad..];
        let (bytes, pad) = dfa.to_bytes_big_endian();
        let be_dfa_bytes = &bytes[pad..];

        quote! {
            impl langen::Tokens for #name {
                fn scan_bytes(input: &[u8]) -> Result<Vec<(Self, langen::Span)>, langen::LexerError> {
                    const DFA: &langen::regex_automata::util::wire::AlignAs<[u8], u32> = &langen::regex_automata::util::wire::AlignAs {
                        _align: [],
                        #[cfg(target_endian = "big")]
                        bytes: [#(#be_dfa_bytes),*],
                        #[cfg(target_endian = "little")]
                        bytes: [#(#le_dfa_bytes),*],
                    };

                    // This is generated above, so we can always safely expect
                    let (dfa, _) = langen::regex_automata::dfa::dense::DFA::from_bytes(&DFA.bytes).expect("Couldn't deserialize dfa");

                    let mut re_input = langen::regex_automata::Input::new(input).anchored(langen::regex_automata::Anchored::Yes);
                    let mut tokens = vec![];
                    let mut current = 0;

                    while current != input.len() {
                        re_input.set_start(current);
                        // Input should always be fine
                        if let Some(m) = dfa.try_search_fwd(&re_input).expect("Regex Error") {
                            let span = langen::Span::new(current, m.offset());
                            current = m.offset();

                            let token = match m.pattern().as_usize() {
                                #(#token_indices => Self::#token_idents,)*
                                _ => {unreachable!("Every pattern has to come from a regex")}
                            };
                            tokens.push((token, span));
                        } else {
                            return Err(langen::LexerError::NoToken(current));
                        }
                    }

                    Ok(tokens)
                }
            }
        }.into()
    } else {
        panic!("Langen can only be used on enum");
    }
}
