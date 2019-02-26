use regex::Regex;

pub use self::Token::{
    Newline,
    Number,
    Operator
};

#[derive(PartialEq, Clone, Debug)]
// TODO: Use diagnostics::Position here instead of repeating line,start,end
pub enum Token {
    Newline {line: usize, start: usize, end: usize},
    Number {line: usize, start: usize, end: usize, value: u32},
    Operator {line: usize, start: usize, end: usize, value: String},
}

//< lexer-tokenize
pub fn tokenize(input: &str) -> Vec<Token> {

    let mut tokens = Vec::new();

    let num_re = Regex::new(r"^(?P<number>\d+)").unwrap();
    let op_re = Regex::new(r"^(?P<operator>[\+*%-])").unwrap();
    let ws_re = Regex::new(r"^(?P<ws>[ \t]+)").unwrap();
    let newline_re = Regex::new(r"^(?P<ws>\n+)").unwrap();

    let mut src = input.clone();
    let mut line = 0;
    let mut offset= 0;
    while src.len() > 0 {
       let (end, tok) =
           if let Some(cap) = num_re.captures(src) {
               match cap.name("number").unwrap().as_str().parse() {
                   Ok(number) => (cap.get(0).unwrap().end(), Some(Number {
                       start: offset + cap.get(0).unwrap().start(),
                       end: offset + cap.get(0).unwrap().end(),
                       value: number,
                       line: line,
                   })),
                   Err(_) => panic!("Lexer failed trying to parse number")
               }
           } else if let Some(cap) = ws_re.captures(src) {
               // Whitespace is simply discarded.
               (cap.get(0).unwrap().end(), None)
           } else if let Some(cap) = op_re.captures(src) {
               (cap.get(0).unwrap().end(), Some(Operator {
                   line: line,
                   start: offset + cap.get(0).unwrap().start(),
                   end: offset + cap.get(0).unwrap().end(),
                   value: cap.name("operator").unwrap().as_str().to_string()
               }))
           } else if let Some(cap) = newline_re.captures(src) {
               let (eidx, tok) = (cap.get(0).unwrap().end(), Some(Newline {
                   line: line,
                   start: offset + cap.get(0).unwrap().start(),
                   end: offset + cap.get(0).unwrap().end(),
               }));
               line += 1;
               offset = 0;
               (eidx, tok)
           } else {
               panic!("Unexpected token(s) in stream: {}; so far: {:?}", src, tokens);
           };
       src = &src[end..];
       offset += end;
       if tok.is_some() { tokens.push(tok.unwrap()); }
    }
    tokens
}
