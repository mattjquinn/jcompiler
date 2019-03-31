use self::AstNode::*;
use itertools::{Itertools};
use pest::error::Error;
use std::fmt;
use std::ffi::CString;

use pest::Parser;

#[derive(Parser)]
#[grammar = "j.pest"]
pub struct JParser;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MonadicVerb {
    Increment = 1,
    Square = 2,
    Negate = 3,
    Reciprocal = 4,
    Tally = 5,
    Ceiling = 6,
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
    Copy = 10,
    LargerOf = 11,
    LargerOrEqual = 12,
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
    Str(CString),
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
            let action = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            parse_monadic_action(action, expr)
        },
        Rule::dyadicExpr => {
            let mut pair = pair.into_inner();
            let lhspair = pair.next().unwrap();
            let lhs = build_ast_from_expr(lhspair);
            let action = pair.next().unwrap();
            let rhspair = pair.next().unwrap();
            let rhs = build_ast_from_expr(rhspair);
            parse_dyadic_action(action, lhs, rhs)
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
        Rule::assgmtExpr => {
            let mut pair = pair.into_inner();
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            AstNode::IsGlobal { ident : String::from(ident.as_str()),
                                expr : Box::new(expr) }
        },
        Rule::string => {
            let str = pair.as_str();
            AstNode::Str(CString::new(&str[1..str.len()-1]).unwrap())
        }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

fn parse_dyadic_action(pair : pest::iterators::Pair<Rule>,
                       lhs : AstNode,
                       rhs : AstNode) -> AstNode {
    let mut pair = pair.into_inner();
    let verb = pair.next().unwrap();
    let adverbs = pair.collect_vec();

    // Adverbs not currently supported on dyadic verbs.
    assert_eq!(adverbs.len(), 0);

    let lhs = Box::new(lhs);
    let rhs = Box::new(rhs);

    match verb.as_str() {
        "+" => AstNode::DyadicOp { verb: DyadicVerb::Plus, lhs, rhs },
        "*" => AstNode::DyadicOp { verb: DyadicVerb::Times, lhs, rhs },
        "-" => AstNode::DyadicOp { verb: DyadicVerb::Minus, lhs, rhs },
        "<" => AstNode::DyadicOp { verb: DyadicVerb::LessThan, lhs, rhs },
        "=" => AstNode::DyadicOp { verb: DyadicVerb::Equal, lhs, rhs },
        ">" => AstNode::DyadicOp { verb: DyadicVerb::LargerThan, lhs, rhs },
        "%" => AstNode::DyadicOp { verb: DyadicVerb::Divide, lhs, rhs },
        "^" => AstNode::DyadicOp { verb: DyadicVerb::Power, lhs, rhs },
        "|" => AstNode::DyadicOp { verb: DyadicVerb::Residue, lhs, rhs },
        "#" => AstNode::DyadicOp { verb: DyadicVerb::Copy, lhs, rhs },
        ">." => AstNode::DyadicOp { verb: DyadicVerb::LargerOf, lhs, rhs },
        ">:" => AstNode::DyadicOp { verb: DyadicVerb::LargerOrEqual, lhs, rhs },
        _ => panic!("Unexpected dyadic verb: {}", verb)
    }
}

fn parse_monadic_action(pair : pest::iterators::Pair<Rule>,
                        expr : AstNode) -> AstNode {
    let mut pair = pair.into_inner();
    let verb = pair.next().unwrap();
    let adverbs = pair.collect_vec();

    match verb.as_str() {
        ">:" => {
            assert_eq!(adverbs.len(), 0);
            AstNode::MonadicOp { verb: MonadicVerb::Increment,
                expr: Box::new(expr) }
        },
        "*:" => {
            assert_eq!(adverbs.len(), 0);
            AstNode::MonadicOp { verb: MonadicVerb::Square,
                expr: Box::new(expr) }
        },
        "-" => {
            match adverbs.len() {
                0 => AstNode::MonadicOp { verb: MonadicVerb::Negate,
                        expr: Box::new(expr) },
                1 => AstNode::Reduce { verb: DyadicVerb::Minus,
                        expr: Box::new(expr) },
                _ => panic!("Unsupported number of adverbs for '-': {}", adverbs.len())
            }
        },
        "%" => {
            assert_eq!(adverbs.len(), 0);
            AstNode::MonadicOp { verb: MonadicVerb::Reciprocal,
                expr: Box::new(expr) }
        },
        "#" => {
            assert_eq!(adverbs.len(), 0);
            AstNode::MonadicOp { verb: MonadicVerb::Tally,
                expr: Box::new(expr) }
        },
        ">." => {
            match adverbs.len() {
                0 => AstNode::MonadicOp { verb: MonadicVerb::Ceiling,
                    expr: Box::new(expr) },
                1 => AstNode::Reduce { verb: DyadicVerb::LargerOf,
                    expr: Box::new(expr) },
                _ => panic!("Unsupported number of adverbs for '>.': {}", adverbs.len())
            }
        },
        "+" => {
            assert_eq!(adverbs.len(), 1);
            assert_eq!(adverbs[0].as_str(), "/");
            AstNode::Reduce { verb: DyadicVerb::Plus,
                expr: Box::new(expr) }
        },
        "*" => {
            assert_eq!(adverbs.len(), 1);
            assert_eq!(adverbs[0].as_str(), "/");
            AstNode::Reduce { verb: DyadicVerb::Times,
                expr: Box::new(expr) }
        },
        _ => panic!("Unsupported monadic action verb: {}", verb.as_str()),
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
