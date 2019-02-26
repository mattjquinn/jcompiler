use regex::Regex;

pub use self::Token::{
    Newline,
    Number,
    Operator
};

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Newline,
    Number(u32),
    Operator(String)
}

//< lexer-tokenize
pub fn tokenize(input: &str) -> Vec<Token> {

    let mut tokens = Vec::new();

    let token_re = Regex::new(concat!(
        r"(?P<number>\d+\.?\d*)|",
        r"(?P<newline>\n)|",
        r"(?P<operator>[\+*%-])")).unwrap();

    for cap in token_re.captures_iter(input) {
        let token = if cap.name("number").is_some() {
            match cap.name("number").unwrap().as_str().parse() {
                Ok(number) => Number(number),
                Err(_) => panic!("Lexer failed trying to parse number")
            }
        } else if cap.name("newline").is_some() {
            Newline
        } else if cap.name("operator").is_some() {
            Operator(cap.name("operator").unwrap().as_str().to_string())
        } else {
           panic!("Lexer expected valid token.");
        };

        tokens.push(token)
    }

    tokens
}
