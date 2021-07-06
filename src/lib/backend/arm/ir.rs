use parser::{DyadicVerb, MonadicVerb};
use super::compiler::{TypedValue};

#[derive(Debug)]
pub enum IRNode {
    PushIntegerOntoStack(i32),
    PushDoublePrecisionFloatOntoStack(f64),
    ApplyMonadicVerbToTypedValue(MonadicVerb, TypedValue),
    ApplyDyadicVerbToTypedValues {verb: DyadicVerb, lhs: TypedValue, rhs: TypedValue },
    ReduceTypedValues(DyadicVerb, Vec<TypedValue>),
    AssignTypedValuesToGlobal {ident: String, values: Vec<TypedValue>}
}
