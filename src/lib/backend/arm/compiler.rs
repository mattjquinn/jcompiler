use parser;
use std::collections::{HashMap, HashSet};

use parser::{AstNode};
use itertools::Itertools;
use itertools::EitherOrBoth::{Both, Left, Right};
use std::cmp::max;

use super::support::{GlobalContext, BasicBlock, Pointer, Type, TypedValue, unify_types};
use backend::arm::ir::IRNode;

pub fn compile_expr(
    globalctx: &mut GlobalContext,
    global_bb: &mut BasicBlock,
    bb: &mut BasicBlock,
    expr: &AstNode) -> Vec<TypedValue>
{
    match expr {
        parser::AstNode::Integer(int) => {
            bb.ir(IRNode::PushIntegerOntoStack(*int), &globalctx)
        },
        parser::AstNode::DoublePrecisionFloat(num) => {
            bb.ir(IRNode::PushDoublePrecisionFloatOntoStack(*num), &globalctx)
        },
        parser::AstNode::Terms(terms) => {
            let mut val_offsets = vec![];
            for term in terms {
                val_offsets.extend(compile_expr(globalctx, global_bb, bb, term));
            }
            val_offsets
        },
        parser::AstNode::MonadicOp {verb, expr} => {
            let vals = compile_expr(globalctx, global_bb, bb, expr);
            let mut out = vec![];
            for val in &vals {
                out.extend(bb.ir(IRNode::ApplyMonadicVerbToTypedValue(verb.clone(), val.clone()), &globalctx));
            }
            out   // this should always be the same as val_offsets because we updated in-place on the stack
        },
        parser::AstNode::DyadicOp {verb, lhs, rhs} => {
            let rhs_offsets = compile_expr(globalctx, global_bb, bb, rhs);
            let lhs_offsets = compile_expr(globalctx, global_bb, bb, lhs);
            if rhs_offsets.len() != lhs_offsets.len()
                && (lhs_offsets.len() != 1 && rhs_offsets.len() != 1) {
                panic!("Dyadic op lhs has length {}, rhs has length {}; don't know how to proceed.", lhs_offsets.len(), rhs_offsets.len())
            }

            // If the LHS and RHS are different lengths, the shorter of the two is repeated to the length of the other.
            let repeated_offset = match lhs_offsets.len() {
                1 => lhs_offsets.get(0).unwrap(),
                _ => rhs_offsets.get(0).unwrap()
            };

            let mut dest_offsets = vec![];
            for pair in lhs_offsets.iter().zip_longest(rhs_offsets.iter()) {
                let (l, r) = match pair {
                    Both(l, r) => (l, r),
                    Left(l) => (l, repeated_offset),
                    Right(r) => (repeated_offset, r)
                };
                dest_offsets.extend(bb.ir(IRNode::ApplyDyadicVerbToTypedValues {verb: verb.clone(), lhs: l.clone(), rhs: r.clone()}, &globalctx));
            }
            dest_offsets
        },
        parser::AstNode::Reduce {verb, expr} => {
            let expr_offsets = compile_expr(globalctx, global_bb, bb, expr);
            bb.ir(IRNode::ReduceTypedValues(verb.clone(), expr_offsets), &globalctx)
        },
        parser::AstNode::GlobalVarAssgmt {ident, expr} => {
            let expr_offsets = compile_expr(globalctx, global_bb, bb, expr);
            let out_offsets = bb.ir(IRNode::AssignTypedValuesToGlobal { ident: ident.clone(), offsets: expr_offsets }, &globalctx);
            globalctx.add_and_set_global_ident_offsets(ident, &out_offsets);
            out_offsets
        },
        parser::AstNode::Ident(ident) =>
            globalctx.global_ident_to_offsets.get(ident).unwrap().clone()
        ,
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}

pub fn register_globals(expr: &AstNode,
                    registered_idents: &mut HashSet<String>,
                    ident_type_map: &mut HashMap<String, Type>) {
    match expr {
        parser::AstNode::Print(expr) =>
            register_globals(expr, registered_idents, ident_type_map),
        parser::AstNode::Integer(_int) => (),
        parser::AstNode::DoublePrecisionFloat(_double) => (),
        parser::AstNode::Terms(terms) =>
            terms.iter().for_each(|e| register_globals(e, registered_idents, ident_type_map)),
        parser::AstNode::MonadicOp {verb: _, expr} =>
            register_globals(expr, registered_idents, ident_type_map),
        parser::AstNode::DyadicOp {verb: _, lhs, rhs} => {
            register_globals(lhs, registered_idents, ident_type_map);
            register_globals(rhs, registered_idents, ident_type_map);
        }
        parser::AstNode::Reduce {verb: _, expr} =>
            register_globals(expr, registered_idents, ident_type_map),
        parser::AstNode::GlobalVarAssgmt {ident, expr} => {
            registered_idents.insert(ident.clone());
            ident_type_map.insert(ident.clone(), determine_type(expr, &ident_type_map).unwrap());
            register_globals(expr, registered_idents, ident_type_map)
        },
        parser::AstNode::Ident(_ident) => (),
        _ => panic!("Not ready to register globals declared in expr: {:?}", expr)
    }
}

fn determine_type(expr: &AstNode, ident_type_map: &HashMap<String, Type>) -> Option<Type> {
    match expr {
        parser::AstNode::Integer(_int) => Some(Type::Integer),
        parser::AstNode::DoublePrecisionFloat(_double) => Some(Type::Double),
        parser::AstNode::Ident(ident) => Some(ident_type_map.get(ident).unwrap().clone()),
        parser::AstNode::DyadicOp{verb: _, lhs, rhs} =>
            Some(unify_types(
                &determine_type(lhs, &ident_type_map).unwrap(),
                &determine_type(rhs, &ident_type_map).unwrap())),
        parser::AstNode::Terms(terms) =>
            Some(Type::Array(terms.len() as u16)),
        _ => panic!("TODO: Unprepared to determine type of {:?}", expr)
    }
}

