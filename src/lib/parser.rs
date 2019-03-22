use self::AstNode::*;
use itertools::Itertools;
use pest::error::Error;
use std::fmt;

use pest::Parser;

#[derive(Parser)]
#[grammar = "j.pest"]
pub struct JParser;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Number(u32),
    Plus {lhs: Box<AstNode>, rhs: Box<AstNode>},
    Minus {lhs: Box<AstNode>, rhs: Box<AstNode>},
    Times {lhs: Box<AstNode>, rhs: Box<AstNode>},
    Terms(Vec<AstNode>),
    Increment(Box<AstNode>),
    Square(Box<AstNode>),
    Negate(Box<AstNode>),
    Reduce { dyadic_verb: String, expr: Box<AstNode> },
    LessThan {lhs: Box<AstNode>, rhs: Box<AstNode>},
    Equal {lhs: Box<AstNode>, rhs: Box<AstNode>},
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
            match verb {
                ">:" => Increment(Box::new(build_ast_from_expr(expr))),
                "*:" => Square(Box::new(build_ast_from_expr(expr))),
                "-" => Negate(Box::new(build_ast_from_expr(expr))),
                _ => panic!("Unsupported monadic verb in expr: {}", verb),
            }
        },
        Rule::dyadicExpr => {
            let mut pair = pair.into_inner();
            let lhspair = pair.next().unwrap();
            let lhs = build_ast_from_expr(lhspair);
            let op = pair.next().unwrap();
            let rhspair = pair.next().unwrap();
            let rhs = build_ast_from_expr(rhspair);
            match op.as_str() {
                "+" => AstNode::Plus{lhs: Box::new(lhs), rhs: Box::new(rhs)},
                "*" => AstNode::Times{lhs: Box::new(lhs), rhs: Box::new(rhs)},
                "-" => AstNode::Minus{lhs: Box::new(lhs), rhs: Box::new(rhs)},
                "<" => AstNode::LessThan{lhs: Box::new(lhs), rhs: Box::new(rhs)},
                "=" => AstNode::Equal{lhs: Box::new(lhs), rhs: Box::new(rhs)},
                 _ => panic!("Unexpected binary op: {}", op.as_str())
            }
        },
        Rule::terms => Terms(pair.into_inner()
                                .map(build_ast_from_term)
                                .collect_vec()),
        Rule::insertExpr => {
            let mut pair = pair.into_inner();
            let dyad_to_insert = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let node = build_ast_from_expr(expr);
            AstNode::Reduce { dyadic_verb : String::from(dyad_to_insert.as_str()),
                              expr : Box::new(node) }
        }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::number => AstNode::Number(pair.as_str().trim().parse().unwrap()),
        Rule::expr => build_ast_from_expr(pair),
        unknown_term => panic!("Unexpected term: {:?}", unknown_term),
    }
}
