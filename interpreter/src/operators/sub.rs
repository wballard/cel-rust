use std::ops;

use cel_parser::{ArithmeticOp, Operator};

use crate::{ExecutionError, FunctionContext, ResolveResult, Value};

/// The all important subtraction operator.
pub fn sub(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    left - right
}

impl ops::Sub<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn sub(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l - r).into(),
            (Value::Duration(l), Value::Duration(r)) => Value::Duration(l - r).into(),
            (Value::Timestamp(l), Value::Duration(r)) => Value::Timestamp(l - r).into(),
            (Value::Timestamp(l), Value::Timestamp(r)) => Value::Duration(l - r).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                Operator::Arithmetic(ArithmeticOp::Subtract),
                left,
                right,
            )),
        }
    }
}
