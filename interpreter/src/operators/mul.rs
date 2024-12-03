use std::ops;

use cel_parser::{ArithmeticOp, Operator};

use crate::{ExecutionError, FunctionContext, ResolveResult, Value};

/// The all important multiplication operator.
pub fn mul(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    left * right
}

impl ops::Mul<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn mul(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l * r).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                Operator::Arithmetic(ArithmeticOp::Multiply),
                left,
                right,
            )),
        }
    }
}
