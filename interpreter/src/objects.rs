use crate::context::Context;
use crate::functions::FunctionContext;
use crate::ExecutionError;
use base64;
use cel_parser::ast::*;
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::{Infallible, TryInto};
use std::fmt::{Display, Formatter};
use std::ops;
use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub list: Arc<Vec<Value>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Map {
    pub map: Arc<HashMap<Key, Value>>,
}

impl PartialOrd for Map {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}

impl Map {
    /// Returns a reference to the value corresponding to the key.
    pub fn get(&self, key: &Key) -> Option<&Value> {
        self.map.get(key)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Ord, Clone, PartialOrd, Serialize, Deserialize)]
pub enum Key {
    Number(Decimal),
    Bool(bool),
    String(String),
}

/// Implement conversions from primitive types to [`Key`]
impl From<String> for Key {
    fn from(v: String) -> Self {
        Key::String(v.clone())
    }
}

impl<'a> From<&'a str> for Key {
    fn from(v: &'a str) -> Self {
        Key::String(v.into())
    }
}

impl From<bool> for Key {
    fn from(v: bool) -> Self {
        Key::Bool(v)
    }
}

impl From<Decimal> for Key {
    fn from(v: Decimal) -> Self {
        Key::Number(v)
    }
}

impl From<u64> for Key {
    fn from(v: u64) -> Self {
        Key::Number(v.into())
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Key::Number(v) => write!(f, "{}", v),
            Key::Bool(v) => write!(f, "{}", v),
            Key::String(v) => write!(f, "{}", v),
        }
    }
}

impl TryInto<Key> for Value {
    type Error = Value;

    #[inline(always)]
    fn try_into(self) -> Result<Key, Self::Error> {
        match self {
            Value::Number(v) => Ok(Key::Number(v)),
            Value::String(v) => Ok(Key::String(v)),
            Value::Bool(v) => Ok(Key::Bool(v)),
            _ => Err(self),
        }
    }
}

impl<K: Into<Key>, V: Into<Value>> From<HashMap<K, V>> for Map {
    fn from(map: HashMap<K, V>) -> Self {
        let mut new_map = HashMap::new();
        for (k, v) in map {
            new_map.insert(k.into(), v.into());
        }
        Map {
            map: Arc::new(new_map),
        }
    }
}

pub trait TryIntoValue {
    type Error: std::error::Error + 'static;
    fn try_into_value(self) -> Result<Value, Self::Error>;
}

impl TryIntoValue for Value {
    type Error = Infallible;
    fn try_into_value(self) -> Result<Value, Self::Error> {
        Ok(self)
    }
}

impl From<Key> for String {
    fn from(key: Key) -> Self {
        match key {
            Key::Number(v) => format!("{}", v),
            Key::Bool(v) => format!("{}", v),
            Key::String(v) => v.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    List(Vec<Value>),
    Map(Map),

    Function(String, Option<Box<Value>>),

    // Atoms
    Number(Decimal),
    String(String),
    Bytes(Vec<u8>),
    Bool(bool),

    Duration(chrono::Duration),

    Timestamp(chrono::DateTime<chrono::Utc>),
    Null,
    Ulid(ulid::Ulid),
}

#[derive(Clone, Copy, Debug)]
pub enum ValueType {
    List,
    Map,
    Function,
    Number,
    String,
    Bytes,
    Bool,
    Duration,
    Timestamp,
    Null,
    Ulid,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::List => write!(f, "list"),
            ValueType::Map => write!(f, "map"),
            ValueType::Function => write!(f, "function"),
            ValueType::Number => write!(f, "number"),
            ValueType::String => write!(f, "string"),
            ValueType::Bytes => write!(f, "bytes"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Duration => write!(f, "duration"),
            ValueType::Timestamp => write!(f, "timestamp"),
            ValueType::Null => write!(f, "null"),
            ValueType::Ulid => write!(f, "ulid"),
        }
    }
}

impl Value {
    pub fn type_of(&self) -> ValueType {
        match self {
            Value::List(_) => ValueType::List,
            Value::Map(_) => ValueType::Map,
            Value::Function(_, _) => ValueType::Function,
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Bytes(_) => ValueType::Bytes,
            Value::Bool(_) => ValueType::Bool,

            Value::Duration(_) => ValueType::Duration,

            Value::Timestamp(_) => ValueType::Timestamp,
            Value::Null => ValueType::Null,
            Value::Ulid(_) => ValueType::Ulid,
        }
    }

    pub fn error_expected_type(&self, expected: ValueType) -> ExecutionError {
        ExecutionError::UnexpectedType {
            got: self.type_of().to_string(),
            want: expected.to_string(),
        }
    }
}

impl From<&Value> for Value {
    fn from(value: &Value) -> Self {
        value.clone()
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Number(value.into())
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value::Number(value.into())
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Number(value.into())
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Number(value.into())
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Value::Number(Decimal::from_f32(value).unwrap())
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Number(Decimal::from_f64(value).unwrap())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Function(a1, a2), Value::Function(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Null, Value::Null) => true,

            (Value::Duration(a), Value::Duration(b)) => a == b,

            (Value::Timestamp(a), Value::Timestamp(b)) => a == b,
            (Value::Ulid(a), Value::Ulid(b)) => a == b,
            (_, _) => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => Some(a.cmp(b)),
            (Value::Bool(a), Value::Bool(b)) => Some(a.cmp(b)),
            (Value::Null, Value::Null) => Some(Ordering::Equal),

            (Value::Duration(a), Value::Duration(b)) => Some(a.cmp(b)),

            (Value::Timestamp(a), Value::Timestamp(b)) => Some(a.cmp(b)),
            (Value::Ulid(a), Value::Ulid(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl From<&Key> for Value {
    fn from(value: &Key) -> Self {
        match value {
            Key::Number(v) => Value::Number(*v),
            Key::Bool(v) => Value::Bool(*v),
            Key::String(v) => Value::String(v.clone()),
        }
    }
}

impl From<Key> for Value {
    fn from(value: Key) -> Self {
        match value {
            Key::Number(v) => Value::Number(v),
            Key::Bool(v) => Value::Bool(v),
            Key::String(v) => Value::String(v),
        }
    }
}

impl From<&Key> for Key {
    fn from(key: &Key) -> Self {
        key.clone()
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::List(v.into_iter().map(|v| v.into()).collect::<Vec<_>>().into())
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Value::String(v.to_string().into())
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(v: Option<T>) -> Self {
        match v {
            Some(v) => v.into(),
            None => Value::Null,
        }
    }
}

impl<K: Into<Key>, V: Into<Value>> From<HashMap<K, V>> for Value {
    fn from(v: HashMap<K, V>) -> Self {
        Value::Map(v.into())
    }
}

impl From<ulid::Ulid> for Value {
    fn from(ulid: ulid::Ulid) -> Self {
        Value::Ulid(ulid)
    }
}
use base64::{
    alphabet,
    engine::{self, general_purpose},
    Engine as _,
};

const CUSTOM_ENGINE: engine::GeneralPurpose =
    engine::GeneralPurpose::new(&alphabet::URL_SAFE, general_purpose::NO_PAD);

impl From<Value> for String {
    fn from(value: Value) -> Self {
        match value {
            Value::String(v) => v.clone(),
            Value::Bool(v) => format!("{}", v),
            Value::Number(v) => format!("{}", v),
            Value::Null => "".to_string(),
            Value::Duration(v) => v.to_string(),
            Value::Timestamp(v) => v.to_rfc3339(),
            Value::Ulid(v) => v.to_string(),
            Value::Bytes(v) => {
                let mut buf = String::new();
                CUSTOM_ENGINE.encode_string(v, &mut buf);
                buf
            }
            Value::List(v) => {
                let mut buf = String::new();
                buf.push('[');
                for (i, v) in v.iter().enumerate() {
                    if i > 0 {
                        buf.push_str(", ");
                    }
                    buf.push_str(&String::from(v.clone()));
                }
                buf.push(']');
                buf
            }
            Value::Map(v) => {
                let mut buf = String::new();
                buf.push('{');
                for (i, (k, v)) in v.map.iter().enumerate() {
                    if i > 0 {
                        buf.push_str(", ");
                    }
                    buf.push_str(&String::from(k.clone()));
                    buf.push_str(": ");
                    buf.push_str(&String::from(v.clone()));
                }
                buf.push('}');
                buf
            }
            _ => "".to_string(),
        }
    }
}

impl From<ExecutionError> for ResolveResult {
    fn from(value: ExecutionError) -> Self {
        Err(value)
    }
}

pub type ResolveResult = Result<Value, ExecutionError>;

impl From<Value> for ResolveResult {
    fn from(value: Value) -> Self {
        Ok(value)
    }
}

impl<'a> Value {
    pub fn resolve_all(expr: &[Expression], ctx: &Context) -> ResolveResult {
        let mut res = Vec::with_capacity(expr.len());
        for expr in expr {
            res.push(Value::resolve(expr, ctx)?);
        }
        Ok(Value::List(res.into()))
    }

    #[inline(always)]
    pub fn resolve(expr: &'a Expression, ctx: &Context) -> ResolveResult {
        match expr {
            Expression::Atom(atom) => Ok(atom.into()),
            Expression::Arithmetic(left, op, right) => {
                let left = Value::resolve(left, ctx)?;
                let right = Value::resolve(right, ctx)?;

                match op {
                    ArithmeticOp::Add => left + right,
                    ArithmeticOp::Subtract => left - right,
                    ArithmeticOp::Divide => left / right,
                    ArithmeticOp::Multiply => left * right,
                    ArithmeticOp::Modulus => left % right,
                }
            }
            Expression::Relation(left, op, right) => {
                let left = Value::resolve(left, ctx)?;
                let right = Value::resolve(right, ctx)?;
                let res = match op {
                    RelationOp::LessThan => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            == Ordering::Less
                    }
                    RelationOp::LessThanEq => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            != Ordering::Greater
                    }
                    RelationOp::GreaterThan => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            == Ordering::Greater
                    }
                    RelationOp::GreaterThanEq => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            != Ordering::Less
                    }
                    RelationOp::Equals => right.eq(&left),
                    RelationOp::NotEquals => right.ne(&left),
                    RelationOp::In => match (left, right) {
                        (Value::String(l), Value::String(r)) => r.contains(&*l),
                        (any, Value::List(v)) => v.contains(&any),
                        (any, Value::Map(m)) => match any.try_into() {
                            Ok(key) => m.map.contains_key(&key),
                            Err(_) => false,
                        },
                        (left, right) => Err(ExecutionError::ValuesNotComparable(left, right))?,
                    },
                };
                Value::Bool(res).into()
            }
            Expression::Ternary(cond, left, right) => {
                let cond = Value::resolve(cond, ctx)?;
                if cond.to_bool() {
                    Value::resolve(left, ctx)
                } else {
                    Value::resolve(right, ctx)
                }
            }
            Expression::Or(left, right) => {
                let left = Value::resolve(left, ctx)?;
                if left.to_bool() {
                    left.into()
                } else {
                    Value::resolve(right, ctx)
                }
            }
            Expression::And(left, right) => {
                let left = Value::resolve(left, ctx)?;
                let right = Value::resolve(right, ctx)?;
                Value::Bool(left.to_bool() && right.to_bool()).into()
            }
            Expression::Unary(op, expr) => {
                let expr = Value::resolve(expr, ctx)?;
                match op {
                    UnaryOp::Not => Ok(Value::Bool(!expr.to_bool())),
                    UnaryOp::DoubleNot => Ok(Value::Bool(expr.to_bool())),
                    UnaryOp::Minus => match expr {
                        Value::Number(i) => Ok(Value::Number(-i)),
                        value => Err(ExecutionError::UnsupportedUnaryOperator("minus", value)),
                    },
                    UnaryOp::DoubleMinus => match expr {
                        Value::Number(_) => Ok(expr),
                        value => Err(ExecutionError::UnsupportedUnaryOperator("negate", value)),
                    },
                }
            }
            Expression::Member(left, right) => {
                let left = Value::resolve(left, ctx)?;
                left.member(right, ctx)
            }
            Expression::List(items) => {
                let list = items
                    .iter()
                    .map(|i| Value::resolve(i, ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Value::List(list.into()).into()
            }
            Expression::Map(items) => {
                let mut map = HashMap::default();
                for (k, v) in items.iter() {
                    let key = Value::resolve(k, ctx)?
                        .try_into()
                        .map_err(ExecutionError::UnsupportedKeyType)?;
                    let value = Value::resolve(v, ctx)?;
                    map.insert(key, value);
                }
                Ok(Value::Map(Map {
                    map: Arc::from(map),
                }))
            }
            Expression::Ident(name) => ctx.get_variable(name),
            Expression::FunctionCall(name, target, args) => {
                if let Expression::Ident(name) = &**name {
                    let func = ctx
                        .get_function(&**name)
                        .ok_or_else(|| ExecutionError::UndeclaredReference(name.clone()))?;
                    match target {
                        None => {
                            let mut ctx =
                                FunctionContext::new(name.clone(), None, ctx, args.clone());
                            func.call_with_context(&mut ctx)
                        }
                        Some(target) => {
                            let mut ctx = FunctionContext::new(
                                name.clone(),
                                Some(Value::resolve(target, ctx)?),
                                ctx,
                                args.clone(),
                            );
                            func.call_with_context(&mut ctx)
                        }
                    }
                } else {
                    Err(ExecutionError::UnsupportedFunctionCallIdentifierType(
                        (**name).clone(),
                    ))
                }
            }
        }
    }

    // >> a(b)
    // Member(Ident("a"),
    //        FunctionCall([Ident("b")]))
    // >> a.b(c)
    // Member(Member(Ident("a"),
    //               Attribute("b")),
    //        FunctionCall([Ident("c")]))

    fn member(self, member: &Member, ctx: &Context) -> ResolveResult {
        match member {
            Member::Index(idx) => {
                let idx = Value::resolve(idx, ctx)?;
                match (self, idx) {
                    (Value::List(items), Value::Number(idx)) => items
                        .get(idx.to_usize().unwrap())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::String(str), Value::Number(idx)) => {
                        let from = idx.to_usize().unwrap_or(0);
                        let to = (idx + dec!(1)).to_usize().unwrap_or(0);
                        match str.get(from..to) {
                            None => Ok(Value::Null),
                            Some(str) => Ok(Value::String(str.to_string())),
                        }
                    }
                    (Value::Map(map), Value::String(property)) => map
                        .get(&property.into())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::Map(map), Value::Bool(property)) => map
                        .get(&property.into())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::Map(map), Value::Number(property)) => map
                        .get(&property.into())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::Map(_), index) => Err(ExecutionError::UnsupportedMapIndex(index)),
                    (Value::List(_), index) => Err(ExecutionError::UnsupportedListIndex(index)),
                    (value, index) => Err(ExecutionError::UnsupportedIndex(value, index)),
                }
            }
            Member::Fields(_) => Err(ExecutionError::UnsupportedFieldsConstruction(
                member.clone(),
            )),
            Member::Attribute(name) => {
                // This will always either be because we're trying to access
                // a property on self, or a method on self.
                let child = match self {
                    Value::Map(ref m) => m.map.get(&name.clone().into()).cloned(),
                    _ => None,
                };

                // If the property is both an attribute and a method, then we
                // give priority to the property. Maybe we can implement lookahead
                // to see if the next token is a function call?
                match (child, ctx.has_function(name)) {
                    (None, false) => ExecutionError::NoSuchKey(name.clone()).into(),
                    (Some(child), _) => child.into(),
                    (None, true) => Value::Function(name.clone(), Some(self.into())).into(),
                }
            }
        }
    }

    #[inline(always)]
    fn to_bool(&self) -> bool {
        match self {
            Value::List(v) => !v.is_empty(),
            Value::Map(v) => !v.map.is_empty(),
            Value::Number(v) => *v != dec!(0),
            Value::String(v) => !v.is_empty(),
            Value::Bytes(v) => !v.is_empty(),
            Value::Bool(v) => *v,
            Value::Null => false,

            Value::Duration(v) => v.num_nanoseconds().map(|n| n != 0).unwrap_or(false),

            Value::Timestamp(v) => v.timestamp_nanos_opt().unwrap_or_default() > 0,
            Value::Function(_, _) => false,
            Value::Ulid(_) => true,
        }
    }
}

impl From<&Atom> for Value {
    #[inline(always)]
    fn from(atom: &Atom) -> Self {
        match atom {
            Atom::Number(v) => Value::Number(*v),
            Atom::String(v) => Value::String(v.clone()),
            Atom::Bytes(v) => Value::Bytes(v.clone()),
            Atom::Bool(v) => Value::Bool(*v),
            Atom::Null => Value::Null,
            Atom::Ulid(v) => Value::Ulid(*v),
            Atom::DateTime(v) => Value::Timestamp(*v),
        }
    }
}

impl ops::Add<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(l || r).into(),
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r).into(),
            (Value::List(l), Value::List(r)) => {
                Value::List(l.iter().chain(r.iter()).cloned().collect::<Vec<_>>()).into()
            }
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
            (Value::Bytes(l), Value::Bytes(r)) => {
                let mut new = Vec::with_capacity(l.len() + r.len());
                new.extend_from_slice(&l);
                new.extend_from_slice(&r);
                Value::Bytes(new).into()
            }
            // Merge two maps should overwrite keys in the left map with the right map
            (Value::Map(l), Value::Map(r)) => {
                let mut new = HashMap::default();
                for (k, v) in l.map.iter() {
                    new.insert(k.clone(), v.clone());
                }
                for (k, v) in r.map.iter() {
                    new.insert(k.clone(), v.clone());
                }
                Value::Map(Map { map: new.into() }).into()
            }
            (Value::Null, Value::Null) => Value::Null.into(),

            (Value::Duration(l), Value::Duration(r)) => Value::Duration(l + r).into(),

            (Value::Timestamp(l), Value::Duration(r)) => Value::Timestamp(l + r).into(),

            (Value::Duration(l), Value::Timestamp(r)) => Value::Timestamp(r + l).into(),
            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "add", left, right,
            )),
        }
    }
}

impl ops::Add<Value> for &Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn add(self, rhs: Value) -> Self::Output {
        let lhs = self.clone();
        lhs.add(rhs)
    }
}

impl ops::Add<&Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn add(self, rhs: &Value) -> Self::Output {
        self.add(rhs.clone())
    }
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
                "sub", left, right,
            )),
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn div(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l / r).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "div", left, right,
            )),
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn mul(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l * r).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "mul", left, right,
            )),
        }
    }
}

impl ops::Rem<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn rem(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l % r).into(),
            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "rem", left, right,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{objects::Key, Context, ExecutionError, Program, Value};
    use std::collections::HashMap;

    #[test]
    fn test_indexed_map_access() {
        let mut context = Context::default();
        let mut headers = HashMap::new();
        headers.insert("Content-Type", "application/json".to_string());
        context.add_variable_from_value("headers", headers);

        let program = Program::compile("headers[\"Content-Type\"]").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, "application/json".into());
    }

    #[test]
    fn test_numeric_map_access() {
        let mut context = Context::default();
        let mut numbers: HashMap<Key, String> = HashMap::new();
        numbers.insert(1.into(), "one".to_string());
        context.add_variable_from_value("numbers", numbers);

        let program = Program::compile("numbers[1]").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, "one".into());
    }

    #[test]
    fn test_heterogeneous_compare() {
        let context = Context::default();

        let program = Program::compile("1.0 < 2").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());

        let program = Program::compile("1 < 1.1").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());

        let program = Program::compile("0 > -10").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(
            value,
            true.into(),
            "negative signed ints should be less than uints"
        );
    }

    #[test]
    fn test_float_compare() {
        let context = Context::default();

        let program = Program::compile("1.0 > 0.0").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());
    }

    #[test]
    fn test_invalid_compare() {
        let context = Context::default();

        let program = Program::compile("{} == []").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, false.into());
    }

    #[test]
    fn test_size_fn_var() {
        let program = Program::compile("size(requests) + size == 5").unwrap();
        let mut context = Context::default();
        let requests = vec![Value::Number(42.into()), Value::Number(42.into())];
        context
            .add_variable("requests", Value::List(requests))
            .unwrap();
        context
            .add_variable("size", Value::Number(3.into()))
            .unwrap();
        assert_eq!(program.execute(&context).unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_add_numbers() {
        let program = Program::compile("1 + 2").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(result.unwrap(), Value::Number(3.into()));
    }

    #[test]
    fn test_add_strings() {
        let program = Program::compile("'foo' + 'bar'").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(result.unwrap(), Value::String("foobar".to_string()));
    }

    #[test]
    fn test_add_lists() {
        let program = Program::compile("[1, 2] + [3, 4]").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(
            result.unwrap(),
            Value::List(vec![1.into(), 2.into(), 3.into(), 4.into()])
        );
    }

    #[test]
    fn test_add_number_to_string() {
        let program = Program::compile("'foo' + 10").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(result.unwrap(), Value::String("foo10".to_string()));
    }

    /// Helper that will expect an error.
    fn test_execution_error(program: &str, expected: ExecutionError) {
        let program = Program::compile(program).unwrap();
        let result = program.execute(&Context::default());
        assert_eq!(result.unwrap_err(), expected);
    }

    #[test]
    fn test_invalid_sub() {
        test_execution_error(
            "'foo' - 10",
            ExecutionError::UnsupportedBinaryOperator(
                "sub",
                "foo".into(),
                Value::Number(10.into()),
            ),
        );
    }

    #[test]
    fn test_invalid_div() {
        test_execution_error(
            "'foo' / 10",
            ExecutionError::UnsupportedBinaryOperator(
                "div",
                "foo".into(),
                Value::Number(10.into()),
            ),
        );
    }

    #[test]
    fn test_invalid_rem() {
        test_execution_error(
            "'foo' % 10",
            ExecutionError::UnsupportedBinaryOperator(
                "rem",
                "foo".into(),
                Value::Number(10.into()),
            ),
        );
    }

    #[test]
    fn out_of_bound_list_access() {
        let program = Program::compile("list[10]").unwrap();
        let mut context = Context::default();
        context.add_variable("list", Value::List(vec![])).unwrap();
        let result = program.execute(&context);
        assert_eq!(result.unwrap(), Value::Null);
    }

    #[test]
    fn reference_to_value() {
        let test = "example".to_string();
        let direct: Value = test.as_str().into();
        assert_eq!(direct, Value::String("example".to_string()));

        let vec = vec![test.as_str()];
        let indirect: Value = vec.into();
        assert_eq!(
            indirect,
            Value::List(vec![Value::String("example".to_string())])
        );
    }

    #[test]
    fn ulid_value() {
        let program = Program::compile("01JDCHE5FVVR8ADKC040GFKZJH").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(
            result.unwrap(),
            Value::Ulid(ulid::Ulid::from_string("01JDCHE5FVVR8ADKC040GFKZJH").unwrap())
        );
    }

    #[test]
    fn ulid_equality() {
        let program =
            Program::compile("01JDCHE5FVVR8ADKC040GFKZJH == 01JDCHE5FVVR8ADKC040GFKZJH").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn ulid_ordering() {
        let program =
            Program::compile("01JDCHE5FVVR8ADKC040GFKZJJ > 01JDCHE5FVVR8ADKC040GFKZJH").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(result.unwrap(), Value::Bool(true));
    }
}
