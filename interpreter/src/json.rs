use crate::Value;
use base64::prelude::*;

use chrono::Duration;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("unable to convert value to json: {0:?}")]
pub enum ConvertToJsonError<'a> {
    /// We cannot convert the CEL value to JSON. Some CEL types (like functions) are
    /// not representable in JSON.
    #[error("unable to convert value to json: {0:?}")]
    Value(&'a Value),

    /// The duration is too large to convert to nanoseconds. Any duration of 2^63
    /// nanoseconds or more will overflow. We'll return the duration type in the
    /// error message.
    #[error("duration too large to convert to nanoseconds: {0:?}")]
    DurationOverflow(&'a Duration),
}

impl Value {
    /// Converts a CEL value to a JSON value.
    ///
    /// # Example
    /// ```
    /// use cel_interpreter::{Context, Program};
    ///
    /// let program = Program::compile("null").unwrap();
    /// let value = program.execute(&Context::default()).unwrap();
    /// let result = value.json().unwrap();
    ///
    /// assert_eq!(result, serde_json::Value::Null);
    /// ```
    pub fn json(&self) -> Result<serde_json::Value, ConvertToJsonError> {
        Ok(match *self {
            Value::List(ref vec) => serde_json::Value::Array(
                vec.iter()
                    .map(|v| v.json())
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Value::Map(ref map) => {
                let mut obj = serde_json::Map::new();
                for (k, v) in map.map.iter() {
                    obj.insert(k.to_string(), v.json()?);
                }
                serde_json::Value::Object(obj)
            }
            Value::Int(i) => i.into(),
            Value::UInt(u) => u.into(),
            Value::Float(f) => f.into(),
            Value::String(ref s) => s.to_string().into(),
            Value::Bool(b) => b.into(),
            Value::Bytes(ref b) => BASE64_STANDARD.encode(b.as_slice()).to_string().into(),
            Value::Null => serde_json::Value::Null,

            Value::Timestamp(ref dt) => dt.to_rfc3339().into(),

            Value::Duration(ref v) => serde_json::Value::Number(serde_json::Number::from(
                v.num_nanoseconds()
                    .ok_or(ConvertToJsonError::DurationOverflow(v))?,
            )),
            _ => return Err(ConvertToJsonError::Value(self)),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::objects::Map;
    use crate::Value as CelValue;

    use chrono::Duration;
    use serde_json::json;
    use std::collections::HashMap;

    #[test]
    fn test_cel_value_to_json() {
        let mut tests = vec![
            (json!("hello"), CelValue::String("hello".to_string().into())),
            (json!(42), CelValue::Int(42)),
            (json!(42.0), CelValue::Float(42.0)),
            (json!(true), CelValue::Bool(true)),
            (json!(null), CelValue::Null),
            (
                json!([true, null]),
                CelValue::List(vec![CelValue::Bool(true), CelValue::Null].into()),
            ),
            (
                json!({"hello": "world"}),
                CelValue::Map(Map::from(HashMap::from([(
                    "hello".to_string(),
                    CelValue::String("world".to_string().into()),
                )]))),
            ),
        ];

        if true {
            tests.push((
                json!(1_000_000_000),
                CelValue::Duration(Duration::seconds(1)),
            ));
        }

        for (expected, value) in tests.iter() {
            assert_eq!(
                value.json().unwrap(),
                *expected,
                "{:?}={:?}",
                value,
                expected
            );
        }
    }
}
