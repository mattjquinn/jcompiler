use self::AstNode::*;
use ascii::AsciiString;
use itertools::Itertools;
use pest::error::Error;
use std::fmt;

use pest::Parser;

#[derive(Parser)]
#[grammar = "j.pest"]
pub struct JParser;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum MonadicVerb {
    Increment = 1,
    Square = 2,
    Negate = 3,
    Reciprocal = 4,
    Tally = 5,
    Ceiling = 6,
    ShapeOf = 7,
    Conjugate = 8,
    Signum = 9,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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
    Shape = 13,
    Append = 14,
}

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Integer(i32),
    DoublePrecisionFloat(f64),
    MonadicOp {
        verb: MonadicVerb,
        expr: Box<AstNode>,
    },
    DyadicOp {
        verb: DyadicVerb,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    Terms(Vec<AstNode>),
    Reduce {
        verb: DyadicVerb,
        expr: Box<AstNode>,
    },
    GlobalVarAssgmt {
        ident: String,
        expr: Box<AstNode>,
    },
    Ident(String),
    Str(AsciiString),
}

#[derive(PartialEq, Debug, Clone)]
pub enum VerbAction {
    MonadicVerbExpr(MonadicVerb, Vec<MonadicAdverb>),
    MonadicHook(DyadicVerb, MonadicVerb),
}

#[derive(PartialEq, Debug, Clone)]
pub enum MonadicAdverb {
    Insert = 1,
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
            let action = parse_verb_action(pair.next().unwrap());
            let expr = build_ast_from_expr(pair.next().unwrap());
            build_ast_node_from_monadic_verb_action(action, expr)
        }
        Rule::dyadicExpr => {
            let mut pair = pair.into_inner();
            let lhspair = pair.next().unwrap();
            let lhs = build_ast_from_expr(lhspair);
            let action = pair.next().unwrap();
            let rhspair = pair.next().unwrap();
            let rhs = build_ast_from_expr(rhspair);
            parse_dyadic_action(action, lhs, rhs)
        }
        Rule::terms => {
            let terms = pair.into_inner().map(build_ast_from_term).collect_vec();
            // If there's just a single term, return it without
            // wrapping it in a Terms node.
            match terms.len() {
                1 => terms.get(0).unwrap().clone(),
                _ => Terms(terms),
            }
        }
        Rule::assgmtExpr => {
            let mut pair = pair.into_inner();
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            AstNode::GlobalVarAssgmt {
                ident: String::from(ident.as_str()),
                expr: Box::new(expr),
            }
        }
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            // Escaped string quotes become single quotes here.
            let str = str.replace("''", "'");
            AstNode::Str(AsciiString::from_ascii(&str[..]).unwrap())
        }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

fn parse_dyadic_action(pair: pest::iterators::Pair<Rule>, lhs: AstNode, rhs: AstNode) -> AstNode {
    let mut pair = pair.into_inner();

    // We currently only have one alternative for dyadicAction
    // (verbExpr) so we just destructure it here without the aid of a separate method.
    let mut verb_expr = pair.next().unwrap().into_inner();
    let verb = verb_expr.next().unwrap();
    let adverbs = verb_expr.collect_vec();

    // Adverbs not currently supported on dyadic verbs.
    assert_eq!(adverbs.len(), 0);

    let lhs = Box::new(lhs);
    let rhs = Box::new(rhs);

    match verb.as_str() {
        "+" => AstNode::DyadicOp {
            verb: DyadicVerb::Plus,
            lhs,
            rhs,
        },
        "*" => AstNode::DyadicOp {
            verb: DyadicVerb::Times,
            lhs,
            rhs,
        },
        "-" => AstNode::DyadicOp {
            verb: DyadicVerb::Minus,
            lhs,
            rhs,
        },
        "<" => AstNode::DyadicOp {
            verb: DyadicVerb::LessThan,
            lhs,
            rhs,
        },
        "=" => AstNode::DyadicOp {
            verb: DyadicVerb::Equal,
            lhs,
            rhs,
        },
        ">" => AstNode::DyadicOp {
            verb: DyadicVerb::LargerThan,
            lhs,
            rhs,
        },
        "%" => AstNode::DyadicOp {
            verb: DyadicVerb::Divide,
            lhs,
            rhs,
        },
        "^" => AstNode::DyadicOp {
            verb: DyadicVerb::Power,
            lhs,
            rhs,
        },
        "|" => AstNode::DyadicOp {
            verb: DyadicVerb::Residue,
            lhs,
            rhs,
        },
        "#" => AstNode::DyadicOp {
            verb: DyadicVerb::Copy,
            lhs,
            rhs,
        },
        ">." => AstNode::DyadicOp {
            verb: DyadicVerb::LargerOf,
            lhs,
            rhs,
        },
        ">:" => AstNode::DyadicOp {
            verb: DyadicVerb::LargerOrEqual,
            lhs,
            rhs,
        },
        "$" => AstNode::DyadicOp {
            verb: DyadicVerb::Shape,
            lhs,
            rhs,
        },
        "," => AstNode::DyadicOp {
            verb: DyadicVerb::Append,
            lhs,
            rhs,
        },
        _ => panic!("Unexpected dyadic verb: {}", verb),
    }
}

fn parse_verb_action(pair: pest::iterators::Pair<Rule>) -> VerbAction {
    let action = pair.into_inner().next().unwrap();
    match action.as_rule() {
        Rule::verbExpr => parse_verb_expr(action),
        Rule::monadicHook => parse_monadic_hook(action),
        unknown => panic!("Unexpected verb action: {:?}", unknown),
    }
}

fn parse_verb_expr(pair: pest::iterators::Pair<Rule>) -> VerbAction {
    let mut pair = pair.into_inner();
    let verb = parse_monadic_verb(pair.next().unwrap());
    let adverbs = pair.map(parse_monadic_adverb).collect_vec();
    VerbAction::MonadicVerbExpr(verb, adverbs)
}

fn parse_monadic_adverb(pair: pest::iterators::Pair<Rule>) -> MonadicAdverb {
    let adverb = pair.as_str();
    match adverb {
        "/" => MonadicAdverb::Insert,
        _ => panic!("MQ: TODO: {:?}", adverb),
    }
}

fn parse_dyadic_verb(pair: pest::iterators::Pair<Rule>) -> DyadicVerb {
    let verb = pair.as_str();
    match verb {
        "%" => DyadicVerb::Divide,
        _ => panic!("Unsupported dyadic verb: {:?}", verb),
    }
}

fn parse_monadic_verb(pair: pest::iterators::Pair<Rule>) -> MonadicVerb {
    let verb = pair.as_str();
    match verb {
        "%" => MonadicVerb::Reciprocal,
        ">:" => MonadicVerb::Increment,
        "*:" => MonadicVerb::Square,
        "-" => MonadicVerb::Negate,
        "#" => MonadicVerb::Tally,
        ">." => MonadicVerb::Ceiling,
        "+" => MonadicVerb::Conjugate,
        "*" => MonadicVerb::Signum,
        "$" => MonadicVerb::ShapeOf,
        _ => panic!("Unsupported monadic verb: {:?}", verb),
    }
}

fn parse_monadic_hook(pair: pest::iterators::Pair<Rule>) -> VerbAction {
    let mut pair = pair.into_inner();
    let f_verb = parse_dyadic_verb(pair.next().unwrap());
    let g_verb = parse_monadic_verb(pair.next().unwrap());
    VerbAction::MonadicHook(f_verb, g_verb)
}

fn build_ast_node_from_monadic_verb_action(action: VerbAction, expr: AstNode) -> AstNode {
    match action {
        VerbAction::MonadicVerbExpr(verb, adverbs) => {
            let expr = Box::new(expr);
            match adverbs[..] {
                [] => AstNode::MonadicOp { verb, expr },
                [MonadicAdverb::Insert] => match verb {
                    MonadicVerb::Negate => AstNode::Reduce {
                        verb: DyadicVerb::Minus,
                        expr,
                    },
                    MonadicVerb::Ceiling => AstNode::Reduce {
                        verb: DyadicVerb::LargerOf,
                        expr,
                    },
                    MonadicVerb::Conjugate => AstNode::Reduce {
                        verb: DyadicVerb::Plus,
                        expr,
                    },
                    MonadicVerb::Signum => AstNode::Reduce {
                        verb: DyadicVerb::Times,
                        expr,
                    },
                    _ => panic!(
                        "Unsupported monadic verb {:?} with adverbs {:?}",
                        verb, adverbs
                    ),
                },
                _ => panic!("Unsupported adverbs: {:?}", adverbs),
            }
        }
        VerbAction::MonadicHook(f_verb, g_verb) => AstNode::DyadicOp {
            verb: f_verb,
            lhs: Box::new(expr.clone()),
            rhs: Box::new(AstNode::MonadicOp {
                verb: g_verb,
                expr: Box::new(expr.clone()),
            }),
        },
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
            let integer: i32 = istr.parse().unwrap();
            AstNode::Integer(sign * integer)
        }
        Rule::decimal => {
            let dstr = pair.as_str();
            let (sign, dstr) = match &dstr[..1] {
                "_" => (-1.0, &dstr[1..]),
                _ => (1.0, &dstr[..]),
            };
            let mut flt: f64 = dstr.parse().unwrap();
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
