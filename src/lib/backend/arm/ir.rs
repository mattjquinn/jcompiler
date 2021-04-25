#[derive(Debug)]
pub enum IRNode {
    PushIntegerOntoStack(i32),
    PushDoublePrecisionFloatOntoStack(f64)
}
