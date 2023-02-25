extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::ItemEnum;
use syn::{self, Attribute};

#[proc_macro_derive(Langen, attributes(token))]
pub fn langen_macro_fn(input: TokenStream) -> TokenStream {
    let base_enum: ItemEnum = syn::parse(input).expect("Langen must be applied to an enum");
    let name = base_enum.ident;

    for variant in &base_enum.variants {
        for attrib in &variant.attrs {
            match attrib.path.get_ident().unwrap().to_string().as_str() {
                "token" => {
                    let regex = parse_token(attrib).expect(
                        format!("Invalid \"token\" argument for \"{}\", expected #[token(\"<regex>\")]", variant.ident).as_str()
                    );
                    println!("{} {}", variant.ident, regex);
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
