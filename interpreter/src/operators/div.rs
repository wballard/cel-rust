use std::ops;

use cel_parser::{ArithmeticOp, Operator};

use crate::{ExecutionError, FunctionContext, ResolveResult, Value};

/// The all important divide operator.
pub fn div(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    left / right
}

impl ops::Div<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn div(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l / r).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                Operator::Arithmetic(ArithmeticOp::Divide),
                left,
                right,
            )),
        }
    }
}
