use super::values::TypedValue;
use parser::{DyadicVerb, MonadicVerb};

#[derive(Debug)]
pub enum IRNode {
    PushIntegerOntoStack(i32),
    PushDoublePrecisionFloatOntoStack(f64),
    ApplyMonadicVerbToTypedValue(MonadicVerb, Box<dyn TypedValue>),
    ApplyDyadicVerbToTypedValues {
        verb: DyadicVerb,
        lhs: Box<dyn TypedValue>,
        rhs: Box<dyn TypedValue>,
    },
    ReduceTypedValues(DyadicVerb, Vec<Box<dyn TypedValue>>),
    AssignTypedValuesToGlobal {
        ident: String,
        values: Vec<Box<dyn TypedValue>>,
    },
}
