use crate::{ExecutionError, FunctionContext, Value};

/// Equality - delegates to `Value`.
pub fn eq(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left == right))
}
