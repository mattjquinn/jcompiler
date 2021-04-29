use parser::{DyadicVerb, MonadicVerb};
use super::support::{Offset};

#[derive(Debug)]
pub enum IRNode {
    PushIntegerOntoStack(i32),
    PushDoublePrecisionFloatOntoStack(f64),
    ApplyMonadicVerbToMemoryOffset(MonadicVerb, Offset),
    ApplyDyadicVerbToMemoryOffsets{verb: DyadicVerb, lhs: Offset, rhs: Offset}
}
