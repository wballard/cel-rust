use std::ops;

use cel_parser::identifiers::HashTag;
use cel_parser::{ArithmeticOp, Operator};

use crate::{ExecutionError, FunctionContext, ResolveResult, Value};

/// The all important add operator.
pub fn add(_: &FunctionContext, left: Value, right: Value) -> Result<Value, ExecutionError> {
    left + right
}

impl ops::Add<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(l || r).into(),
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r).into(),
            (Value::List(l), Value::List(r)) => Value::List(
                l.list
                    .iter()
                    .chain(r.list.iter())
                    .cloned()
                    .collect::<Vec<_>>()
                    .into(),
            )
            .into(),

            // additions that turn into strings
            (Value::String(l), Value::String(r)) => {
                let mut new = String::with_capacity(l.len() + r.len());
                new.push_str(&l);
                new.push_str(&r);
                Value::String(new).into()
            }
            (Value::String(l), Value::Number(r)) => {
                let mut new = String::with_capacity(l.len() + 1);
                new.push_str(&l);
                new.push_str(&r.to_string());
                Value::String(new).into()
            }
            (Value::Number(l), Value::String(r)) => {
                let mut new = String::with_capacity(r.len() + 1);
                new.push_str(&l.to_string());
                new.push_str(&r);
                Value::String(new).into()
            }

            // adding on to hashtags
            (Value::HashTag(l), Value::HashTag(r)) => {
                let mut new = String::with_capacity(l.len() + r.len() + 1);
                new.push_str(l.as_ref());
                new.push_str(r.as_ref());
                Value::HashTag(HashTag::new(new)).into()
            }
            (Value::HashTag(l), Value::String(r)) => {
                let mut new = String::with_capacity(l.len() + r.len() + 1);
                new.push_str(l.as_ref());
                new.push_str(r.as_ref());
                Value::HashTag(HashTag::new(new)).into()
            }

            (Value::Duration(l), Value::Duration(r)) => Value::Duration(l + r).into(),
            (Value::Timestamp(l), Value::Duration(r)) => Value::Timestamp(l + r).into(),
            (Value::Duration(l), Value::Timestamp(r)) => Value::Timestamp(r + l).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                Operator::Arithmetic(ArithmeticOp::Add),
                left,
                right,
            )),
        }
    }
}
