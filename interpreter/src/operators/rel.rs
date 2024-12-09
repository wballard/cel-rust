use crate::{ExecutionError, FunctionContext, Value};

/// Equality - delegates to `Value`.
pub fn eq(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left == right))
}

/// Not equality - delegates to `Value`.
pub fn neq(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left != right))
}

/// Less than - delegates to `Value`.
pub fn lt(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left < right))
}

/// Less than equals - delegates to `Value`.
pub fn lte(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left <= right))
}

/// Greater than - delegates to `Value`.
pub fn gt(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left > right))
}

/// Greater than equals - delegates to `Value`.
pub fn gte(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Bool(left >= right))
}

/// Backward from `contains` --
pub fn is_in(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(match right {
        Value::List(v) => v.list.contains(&left),
        Value::Set(v) => v.set.contains(&left),
        Value::Tuple(v) => v.list.contains(&left),
        Value::String(s) => {
            if let Value::String(arg) = left {
                s.contains(arg.as_str())
            } else {
                false
            }
        }
        _ => false,
    }
    .into())
}

pub fn range(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    Ok(Value::Range(Box::new(left), Box::new(right)))
}
