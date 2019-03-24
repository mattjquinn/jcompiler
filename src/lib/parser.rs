use self::AstNode::*;
use itertools::Itertools;
use pest::error::Error;
use std::fmt;

use pest::Parser;

#[derive(Parser)]
#[grammar = "j.pest"]
pub struct JParser;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MonadicVerb {
    Increment = 1,
    Square = 2,
    Negate = 3,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum DyadicVerb {
    Plus = 1,
    Times = 2,
    LessThan = 3,
    LargerThan = 4,
    Equal = 5,
    Minus = 6,
    Divide = 7,
    Power = 8,
    Residue = 9,
}

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Integer(i32),
    DoublePrecisionFloat(f64),
    MonadicOp { verb: MonadicVerb, expr: Box<AstNode> },
    DyadicOp { verb: DyadicVerb, lhs: Box<AstNode>, rhs: Box<AstNode>},
    Terms(Vec<AstNode>),
    Reduce { verb: DyadicVerb, expr: Box<AstNode> },
    IsGlobal{ident: String, expr: Box<AstNode>},
    Ident(String),
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(f, "{:?}", self);
        Ok(())
    }
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = vec![];

    let pairs = JParser::parse(Rule::program, source)?;
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        //        println!("Rule:    {:?}", pair.as_rule());
        //        println!("Span:    {:?}", pair.as_span());
        //        println!("Text:    {}", pair.as_str());

        match pair.as_rule() {
            Rule::expr => {
                ast.push(Print(Box::new(build_ast_from_expr(pair))));
            }
            _ => {}
        }
    }

    Ok(ast)
}

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    // IMPORTANT: Binary operations are right-associative in J, i.e.:
    // 3 * 2 + 1  is evaluated as 3 * (2 + 1)

    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::monadicExpr => {
            let mut pair = pair.into_inner();
            let verb = pair.next().unwrap().as_str();
            let expr = pair.next().unwrap();
            let op = monadic_verb_as_enum(verb);
            AstNode::MonadicOp { verb: op, expr: Box::new(build_ast_from_expr(expr))}
        },
        Rule::dyadicExpr => {
            let mut pair = pair.into_inner();
            let lhspair = pair.next().unwrap();
            let lhs = build_ast_from_expr(lhspair);
            let op = pair.next().unwrap();
            let rhspair = pair.next().unwrap();
            let rhs = build_ast_from_expr(rhspair);
            let op = dyadic_verb_as_enum(op.as_str());
            AstNode::DyadicOp { verb: op, lhs: Box::new(lhs), rhs: Box::new(rhs)}
        },
        Rule::terms => {
            let terms = pair.into_inner()
                .map(build_ast_from_term)
                .collect_vec();
            // If there's just a single term, return it without
            // wrapping it in a Terms node.
            match terms.len() {
                1 => terms.get(0).unwrap().clone(),
                _ => Terms(terms),
            }
        },
        Rule::insertExpr => {
            let mut pair = pair.into_inner();
            let dyad_to_insert = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let node = build_ast_from_expr(expr);
            let op = dyadic_verb_as_enum(dyad_to_insert.as_str());
            AstNode::Reduce { verb: op, expr : Box::new(node) }
        },
        Rule::assgmtExpr => {
            let mut pair = pair.into_inner();
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            AstNode::IsGlobal { ident : String::from(ident.as_str()),
                                expr : Box::new(expr) }
        },
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

fn dyadic_verb_as_enum(verb : &str) -> DyadicVerb {
    match verb {
        "+" => DyadicVerb::Plus,
        "*" => DyadicVerb::Times,
        "-" => DyadicVerb::Minus,
        "<" => DyadicVerb::LessThan,
        "=" => DyadicVerb::Equal,
        ">" => DyadicVerb::LargerThan,
        "%" => DyadicVerb::Divide,
        "^" => DyadicVerb::Power,
        "|" => DyadicVerb::Residue,
        _ => panic!("Unexpected dyadic verb: {}", verb)
    }
}

fn monadic_verb_as_enum(verb : &str) -> MonadicVerb {
    match verb {
        ">:" => MonadicVerb::Increment,
        "*:" => MonadicVerb::Square,
        "-" =>  MonadicVerb::Negate,
        _ => panic!("Unsupported monadic verb: {}", verb),
    }
}

fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::integer => {
            let istr = pair.as_str();
            let (sign, istr) = match &istr[..1] {
                "_" => (-1, &istr[1..]),
                _ => (1, &istr[..]),
            };
            let integer : i32 = istr.parse().unwrap();
            AstNode::Integer(sign * integer)
        },
        Rule::decimal => {
            let dstr = pair.as_str();
            let (sign, dstr) = match &dstr[..1] {
                "_" => (-1.0, &dstr[1..]),
                _ => (1.0, &dstr[..]),
            };
            let mut flt : f64 = dstr.parse().unwrap();
            if flt != 0.0 {
                // Avoid negative zeroes; only multiply sign by nonzeroes.
                flt *= sign;
            }
            AstNode::DoublePrecisionFloat(flt)
        }
        Rule::expr => build_ast_from_expr(pair),
        Rule::ident => AstNode::Ident(String::from(pair.as_str())),
        unknown_term => panic!("Unexpected term: {:?}", unknown_term),
    }
}
