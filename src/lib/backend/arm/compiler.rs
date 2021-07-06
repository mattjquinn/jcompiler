use parser;

use parser::{AstNode};
use itertools::Itertools;
use itertools::EitherOrBoth::{Both, Left, Right};

use super::support::{GlobalContext, BasicBlock, TypedValue};
use backend::arm::ir::IRNode;

pub fn compile_expr(
    globalctx: &mut GlobalContext,
    bb: &mut BasicBlock,
    expr: &AstNode) -> Vec<TypedValue>
{
    match expr {
        parser::AstNode::Integer(int) => {
            bb.ir(IRNode::PushIntegerOntoStack(*int), globalctx)
        },
        parser::AstNode::DoublePrecisionFloat(num) => {
            bb.ir(IRNode::PushDoublePrecisionFloatOntoStack(*num), globalctx)
        },
        parser::AstNode::Terms(terms) => {
            let mut values = vec![];
            for term in terms {
                values.extend(compile_expr(globalctx, bb, term));
            }
            values
        },
        parser::AstNode::MonadicOp {verb, expr} => {
            let vals = compile_expr(globalctx, bb, expr);
            let mut out = vec![];
            for val in &vals {
                out.extend(bb.ir(IRNode::ApplyMonadicVerbToTypedValue(*verb, val.clone()), globalctx));
            }
            out   // this should always be the same as vals because we updated in-place on the stack
        },
        parser::AstNode::DyadicOp {verb, lhs, rhs} => {
            let rhs_values = compile_expr(globalctx, bb, rhs);
            let lhs_values = compile_expr(globalctx, bb, lhs);
            if rhs_values.len() != lhs_values.len()
                && (lhs_values.len() != 1 && rhs_values.len() != 1) {
                panic!("Dyadic op lhs has length {}, rhs has length {}; don't know how to proceed.", lhs_values.len(), rhs_values.len())
            }

            // If the LHS and RHS are different lengths, the shorter of the two is repeated to the length of the other.
            let repeated_value = match lhs_values.len() {
                1 => lhs_values.get(0).unwrap(),
                _ => rhs_values.get(0).unwrap()
            };

            let mut dest_values = vec![];
            for pair in lhs_values.iter().zip_longest(rhs_values.iter()) {
                let (l, r) = match pair {
                    Both(l, r) => (l, r),
                    Left(l) => (l, repeated_value),
                    Right(r) => (repeated_value, r)
                };
                dest_values.extend(bb.ir(IRNode::ApplyDyadicVerbToTypedValues {verb: *verb, lhs: l.clone(), rhs: r.clone()}, globalctx));
            }
            dest_values
        },
        parser::AstNode::Reduce {verb, expr} => {
            let values = compile_expr(globalctx, bb, expr);
            bb.ir(IRNode::ReduceTypedValues(*verb, values), globalctx)
        },
        parser::AstNode::GlobalVarAssgmt {ident, expr} => {
            let values = compile_expr(globalctx, bb, expr);
            let heap_values = bb.ir(IRNode::AssignTypedValuesToGlobal { ident: ident.clone(), values }, globalctx);
            globalctx.set_ident_values(ident, &heap_values);
            heap_values
        },
        parser::AstNode::Ident(ident) => globalctx.get_ident_values(ident),
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}
