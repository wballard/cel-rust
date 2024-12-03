use cel_parser::{ArithmeticOp, Operator};
use rust_decimal::MathematicalOps;

use crate::{ExecutionError, FunctionContext, Value};

/// The all important exponentiation operator.
pub fn pow(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    match (left, right) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l.powd(r)).into(),

        (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
            Operator::Arithmetic(ArithmeticOp::Multiply),
            left,
            right,
        )),
    }
}
