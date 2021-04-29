use parser;
use std::collections::{HashMap, HashSet};

use parser::{AstNode, DyadicVerb};
use itertools::Itertools;
use itertools::EitherOrBoth::{Both, Left, Right};
use std::cmp::max;

use super::instructions::{ArmIns};
use super::support::{GlobalContext, BasicBlock, Offset, Type, unify_types};
use backend::arm::ir::IRNode;

pub fn compile_expr(
    globalctx: &mut GlobalContext,
    global_bb: &mut BasicBlock,
    bb: &mut BasicBlock,
    expr: &AstNode) -> Vec<Offset>
{
    match expr {
        parser::AstNode::Integer(int) => {
            bb.ir(IRNode::PushIntegerOntoStack(*int))
        },
        parser::AstNode::DoublePrecisionFloat(num) => {
            bb.ir(IRNode::PushDoublePrecisionFloatOntoStack(*num))
        },
        parser::AstNode::Terms(terms) => {
            let mut val_offsets = vec![];
            for term in terms {
                val_offsets.extend(compile_expr(globalctx, global_bb, bb, term));
            }
            val_offsets
        },
        parser::AstNode::MonadicOp {verb, expr} => {
            let val_offsets = compile_expr(globalctx, global_bb, bb, expr);
            let mut out_offsets = vec![];
            for offset in &val_offsets {
                out_offsets.extend(bb.ir(IRNode::ApplyMonadicVerbToMemoryOffset(verb.clone(), offset.clone())));
            }
            out_offsets   // this should always be the same as val_offsets because we updated in-place on the stack
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
                dest_offsets.extend(bb.ir(IRNode::ApplyDyadicVerbToMemoryOffsets{verb: verb.clone(), lhs: l.clone(), rhs: r.clone()}));
            }
            dest_offsets
        },
        parser::AstNode::Reduce {verb, expr} => {
            bb.instructions.push(ArmIns::Nop);
            let expr_offsets = compile_expr(globalctx, global_bb, bb, expr);
            bb.instructions.push(ArmIns::Nop);
            // Initialize the accumulator to expr's last offset value
            let accum_offset = expr_offsets.last().unwrap();
            match accum_offset {
                Offset::Stack(_type, i) =>
                    bb.instructions.push(ArmIns::LoadOffsetDeprecated {
                        dst: "r3", src: "fp", offsets: vec![*i] }),
                Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
            }
            // Accumulate from right to left.
            for offset_idx in expr_offsets[0..expr_offsets.len()-1].iter().rev() {
                match offset_idx {
                    Offset::Stack(_type, i) =>
                        bb.instructions.push(ArmIns::LoadOffsetDeprecated {
                            dst: "r4", src: "fp", offsets: vec![*i] }),
                    Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
                };
                match verb {
                    DyadicVerb::Plus => {
                        bb.instructions.push(ArmIns::AddDeprecated {
                            dst: "r3",
                            src: "r4",
                            add: "r3"
                        });
                    },
                    DyadicVerb::Minus => {
                        bb.instructions.push(ArmIns::SubDeprecated {
                            dst: "r3",
                            src: "r4",
                            sub: "r3"
                        });
                    },
                    DyadicVerb::Times => {
                        bb.instructions.push(ArmIns::MultiplyDeprecated {
                            dst: "r3",
                            src: "r4",
                            mul: "r3"
                        });
                    },
                    _ => unimplemented!("TODO: Support reduction of monadic verb: {:?}", verb)
                }
            }
            // Store the accumulator in expr's first offset, and return that
            // single offset here.
            match accum_offset {
                Offset::Stack(_type, i) =>
                    bb.instructions.push(ArmIns::StoreOffsetDeprecated {
                        src: "r3", dst: "fp", offsets: vec![*i] }),
                Offset::Global(_type, _ident) => unimplemented!("TODO: Support store to global.")
            };
            vec![accum_offset.clone()]
        },
        parser::AstNode::GlobalVarAssgmt {ident, expr} => {
            let expr_offsets = compile_expr(globalctx, global_bb, bb, expr);

            let mut out_offsets = vec![];
            let mut idx = 0;
            for offset in expr_offsets.iter() {
                match offset {
                    Offset::Stack(_type, i) =>
                        bb.instructions.push(ArmIns::LoadOffsetDeprecated {
                            dst: "r2",
                            src: "fp",
                            offsets: vec![*i]
                        }),
                    Offset::Global(_type, global_ident) => {
                        bb.instructions.push(ArmIns::LoadDeprecated {
                            dst: "r2".to_string(),
                            src: format!("{}", global_ident).to_string()
                        });
                        bb.instructions.push(ArmIns::LoadDeprecated {
                            dst: "r2".to_string(),
                            src: "[r2]".to_string(),
                        });
                    }
                };
                bb.instructions.push(ArmIns::LoadDeprecated {
                    src: format!(".{}_idx{}", ident, idx),
                    dst: "r3".to_string(),
                });
                idx += 1;
                bb.instructions.push(ArmIns::StoreDeprecated {
                    src: "r2".to_string(),
                    dst: "r3".to_string(),
                });
                println!("In GlobalVarAssgmt, adding offset: {:?}", offset);
                out_offsets.push(offset.clone())
            }
            globalctx.add_and_set_global_ident_offsets(ident, &out_offsets);
            out_offsets
        },
        parser::AstNode::Ident(ident) =>
            globalctx.global_ident_to_offsets.get(ident).unwrap().clone()
        ,
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}

pub fn compute_frame_size(expr: &AstNode) -> i32 {
    match expr {
        parser::AstNode::Integer(_int) => 4,
        parser::AstNode::DoublePrecisionFloat(_double) => 8,
        parser::AstNode::Terms(terms) =>
            terms.iter().map(|e| compute_frame_size(e)).sum(),
        parser::AstNode::MonadicOp {verb: _, expr} =>
            compute_frame_size(expr),
        parser::AstNode::DyadicOp {verb: _, lhs, rhs} => {
            let lhs_size = compute_frame_size(lhs);
            let rhs_size = compute_frame_size(rhs);
            // the max term is where we'll store the intermediates; ideally
            // we'd optimize this by overwriting memory addresses that we know are
            // no longer used, but this requires care b/c we can't appropriate globals.
            lhs_size + rhs_size + max(lhs_size, rhs_size)
        }
        parser::AstNode::Reduce {verb: _, expr} =>
            compute_frame_size(expr),
        parser::AstNode::GlobalVarAssgmt {ident: _, expr} =>
            compute_frame_size(expr),
        parser::AstNode::Ident(_ident) => 0,
        _ => panic!("Not ready to compute frame size of expr: {:?}", expr)
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

