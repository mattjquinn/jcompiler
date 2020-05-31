extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::{TokenStream};
use proc_macro2::Span;
use quote::quote;
use std::iter::FromIterator;
use syn::{Token, Lit};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use std::collections::HashSet;

struct TestInput {
    known_test_failures: Punctuated<Lit, Token![,]>,
}

impl Parse for TestInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::bracketed!(content in input);
        Ok(TestInput{
            known_test_failures: content.parse_terminated(Lit::parse)?
        })
    }
}

#[proc_macro]
pub fn generate_tests(input: TokenStream) -> TokenStream {

    let test_input = syn::parse_macro_input!(input as TestInput);
    let mut known_test_failures : HashSet<String> = test_input.known_test_failures.iter().filter_map(
        |s| {
            match s {
                Lit::Str(a) => {
                    Some(a.value())
                },
                _ => {
                    println!("Warning: ignoring non-string literal in KTF list.");
                    None
                }
            }
        }
    ).collect();

    let mut entries: Vec<String> = std::fs::read_dir("jlang_programs").expect("dir")
        .map(|res| res.map(|e| e.path()))
        .filter_map(|p| {
            if !p.is_ok() { panic!("A PathBuf is wrapped in an error.") }
            let pathbuf = p.expect("pathbuf");
            let filename = pathbuf.file_name().expect("filename").to_str().expect("str");
            if !filename.ends_with(".ijs") { panic!("A test file in the test directory doesn't end in .ijs as it should.") }
            if known_test_failures.contains(filename) {
                println!("Ignoring known test failure: {}", filename);
                known_test_failures.remove(filename);
                return None
            }
            return Some(filename.to_string())
        })
        .collect();
    entries.sort();

    if known_test_failures.len() > 0 {
        panic!("One or more KTFs didn't match an actual test file: {:?}", known_test_failures)
    }

    let mut streams : Vec<TokenStream> = vec![];
    let mut idx = 0;

    entries.iter().for_each(|test_filename| {
        let test_name = &test_filename[..&test_filename.len()-4];

        let filename = proc_macro2::Literal::string(test_filename);
        let methodname = proc_macro2::Ident::new(test_name, Span::call_site());
        streams.push(
            (quote! {
                #[test]
                fn #methodname() {
                    common::test(#filename, &compile);
                }
            }).into()
        );
        idx += 1;
    });
    TokenStream::from_iter(streams.into_iter())
}

