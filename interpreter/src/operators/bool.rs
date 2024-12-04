use crate::{ExecutionError, FunctionContext, Value};

pub fn and(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left.to_bool() && right.to_bool()))
}

pub fn or(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left.to_bool() || right.to_bool()))
}

pub fn xor(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left.to_bool() ^ right.to_bool()))
}

pub fn not(_: &FunctionContext, left: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(!left.to_bool()))
}
