extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::{TokenStream};
use proc_macro2::{Span, Literal, Ident};
use quote::quote;
use std::iter::FromIterator;
use syn::{Token, Lit};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use std::collections::HashSet;

struct TestInput {
    known_test_failures: HashSet<String>
}

impl Parse for TestInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::bracketed!(content in input);
        let inner_tokens : Punctuated<Lit, Token![,]> = content.parse_terminated(Lit::parse)?;
        Ok(TestInput{
            known_test_failures: inner_tokens.iter().filter_map(
            |s| {
                match s {
                    Lit::Str(a) => Some(a.value()),
                    _ => {
                        println!("Warning: ignoring non-string literal in KTF list.");
                        None
                    }
                }
            }).collect()
        })
    }
}

#[proc_macro]
pub fn generate_tests(input: TokenStream) -> TokenStream {

    let mut test_input = syn::parse_macro_input!(input as TestInput);

    let mut entries: Vec<String> = std::fs::read_dir("jlang_programs").expect("dir")
        .map(|res| res.map(|e| e.path()))
        .filter_map(|p| {
            if !p.is_ok() { panic!("A PathBuf is wrapped in an error.") }
            let pathbuf = p.expect("pathbuf");
            let filename = pathbuf.file_name().expect("filename").to_str().expect("str");
            if !filename.ends_with(".ijs") { panic!("A test file in the test directory doesn't end in .ijs as it should.") }
            if test_input.known_test_failures.contains(filename) {
                println!("Ignoring known test failure: {}", filename);
                test_input.known_test_failures.remove(filename);
                return None
            }
            return Some(filename.to_string())
        })
        .collect();
    entries.sort();

    if test_input.known_test_failures.len() > 0 {
        panic!("One or more KTFs didn't match an actual test file: {:?}", test_input.known_test_failures)
    }

    let mut streams : Vec<TokenStream> = vec![];
    entries.iter().for_each(|test_filename| {
        let test_name = &test_filename[..&test_filename.len()-4];

        let filename = Literal::string(test_filename);
        let methodname = Ident::new(test_name, Span::call_site());
        streams.push(
            (quote! {
                #[test]
                fn #methodname() {
                    common::test(#filename, &compile);
                }
            }).into()
        );
    });
    TokenStream::from_iter(streams.into_iter())
}

