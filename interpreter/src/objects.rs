use crate::context::Context;
use crate::functions::FunctionContext;
use crate::Atom;
use crate::ExecutionError;
use base64;
use cel_parser::*;
use chrono::SecondsFormat;
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};
use std::ops;

#[derive(Debug, PartialEq, Clone, Hash, PartialOrd, Ord, Eq)]
pub struct ValueList {
    pub list: Vec<Value>,
}

impl From<Vec<Value>> for ValueList {
    fn from(list: Vec<Value>) -> Self {
        ValueList { list }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, PartialOrd, Ord, Eq)]
pub struct ValueSet {
    pub set: BTreeSet<Value>,
}

impl From<Vec<Value>> for ValueSet {
    fn from(list: Vec<Value>) -> Self {
        ValueSet {
            set: list.into_iter().collect(),
        }
    }
}

impl From<Vec<HashTag>> for ValueSet {
    fn from(list: Vec<HashTag>) -> Self {
        ValueSet {
            set: list.into_iter().map(Value::HashTag).collect(),
        }
    }
}

/// Values that can be computed by the interpreter.
#[derive(Debug, Clone)]
pub enum Value {
    List(ValueList),
    Tuple(ValueList),
    Set(ValueSet),

    Function(Identifier, Option<Box<Value>>),

    // Atoms
    Number(Decimal),
    String(String),
    Bytes(Vec<u8>),
    Bool(bool),
    Duration(chrono::Duration),
    Timestamp(chrono::DateTime<chrono::Utc>),
    Null,
    Ulid(ulid::Ulid),
    HashTag(HashTag),
}

/// Reverse type mapping to allow type inspection.
#[derive(Clone, Copy, Debug)]
pub enum ValueType {
    List,
    Tuple,
    Function,
    Number,
    String,
    Bytes,
    Bool,
    Duration,
    Timestamp,
    Null,
    Ulid,
    Tag,
    TagSet,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::List => write!(f, "list"),
            ValueType::Tuple => write!(f, "tuple"),
            ValueType::TagSet => write!(f, "set"),
            ValueType::Function => write!(f, "function"),
            ValueType::Number => write!(f, "number"),
            ValueType::String => write!(f, "string"),
            ValueType::Bytes => write!(f, "bytes"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Duration => write!(f, "duration"),
            ValueType::Timestamp => write!(f, "timestamp"),
            ValueType::Null => write!(f, "null"),
            ValueType::Ulid => write!(f, "ulid"),
            ValueType::Tag => write!(f, "tag"),
        }
    }
}

impl Value {
    pub fn type_of(&self) -> ValueType {
        match self {
            Value::List(_) => ValueType::List,
            Value::Tuple(_) => ValueType::Tuple,
            Value::Function(_, _) => ValueType::Function,
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Bytes(_) => ValueType::Bytes,
            Value::Bool(_) => ValueType::Bool,
            Value::Duration(_) => ValueType::Duration,
            Value::Timestamp(_) => ValueType::Timestamp,
            Value::Null => ValueType::Null,
            Value::Ulid(_) => ValueType::Ulid,
            Value::HashTag(_) => ValueType::Tag,
            Value::Set(_) => ValueType::TagSet,
        }
    }

    pub fn error_expected_type(&self, expected: ValueType) -> ExecutionError {
        ExecutionError::UnexpectedType {
            got: self.type_of().to_string(),
            want: expected.to_string(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(v) => {
                write!(f, "[")?;
                for (i, v) in v.list.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Tuple(v) => {
                write!(f, "(")?;
                for (i, v) in v.list.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "(")
            }
            Value::Function(name, target) => match target {
                Some(target) => write!(f, "{}.{}", target, name),
                None => write!(f, "{}", name),
            },
            Value::Number(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Bytes(v) => {
                let mut buf = String::new();
                CUSTOM_ENGINE.encode_string(v, &mut buf);
                write!(f, "{}", buf)
            }
            Value::Bool(v) => write!(f, "{}", v),
            Value::Null => write!(f, "null"),
            Value::Duration(v) => write!(f, "{}", v),
            Value::Timestamp(v) => write!(f, "{}", v.to_rfc3339_opts(SecondsFormat::Millis, true)),
            Value::Ulid(v) => write!(f, "{}", v),
            Value::HashTag(v) => write!(f, "{}", v),
            Value::Set(v) => {
                write!(f, "{{#")?;
                for (i, v) in v.set.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "}}#")
            }
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
            (Value::HashTag(a), Value::HashTag(b)) => a == b,
            (Value::Set(a), Value::Set(b)) => a == b,
            (_, _) => false,
        }
    }
}

impl Eq for Value {}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::List(list) => list.hash(state),
            Value::Tuple(list) => list.hash(state),
            Value::Function(name, target) => {
                name.hash(state);
                target.hash(state);
            }
            Value::Number(v) => v.hash(state),
            Value::String(v) => v.hash(state),
            Value::Bytes(v) => v.hash(state),
            Value::Bool(v) => v.hash(state),
            Value::Null => Value::Null.hash(state),

            Value::Duration(v) => v.hash(state),

            Value::Timestamp(v) => v.hash(state),
            Value::Ulid(v) => v.hash(state),
            Value::HashTag(v) => v.hash(state),
            Value::Set(v) => v.hash(state),
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.cmp(b),
            (Value::String(a), Value::String(b)) => a.cmp(b),
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Null, Value::Null) => Ordering::Equal,
            (Value::Duration(a), Value::Duration(b)) => a.cmp(b),
            (Value::Timestamp(a), Value::Timestamp(b)) => a.cmp(b),
            (Value::Ulid(a), Value::Ulid(b)) => a.cmp(b),
            (Value::HashTag(a), Value::HashTag(b)) => a.cmp(b),
            _ => Ordering::Equal,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::List(ValueList {
            list: v.into_iter().map(|v| v.into()).collect::<Vec<_>>(),
        })
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Value::String(v.to_string())
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
                for (i, v) in v.list.iter().enumerate() {
                    if i > 0 {
                        buf.push_str(", ");
                    }
                    buf.push_str(&String::from(v.clone()));
                }
                buf.push(']');
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
            /*
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
                        (any, Value::List(v)) => v.list.contains(&any),
                        //TODO: implement for TagSet
                        (left, right) => Err(ExecutionError::ValuesNotComparable(left, right))?,
                    },
                    RelationOp::MemberOf => match (left, right) {
                        _ => unimplemented!(),
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
            */
            Expression::List(items) => {
                let list = items
                    .iter()
                    .map(|i| Value::resolve(i, ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Value::List(list.into()).into()
            }
            Expression::Tuple(items) => {
                let list = items
                    .iter()
                    .map(|i| Value::resolve(i, ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Value::Tuple(list.into()).into()
            }
            Expression::Set(items) => {
                // reduce the expressions to a list of tag values
                let list: Vec<_> = items
                    .iter()
                    .flat_map(|i| Value::resolve(i, ctx))
                    .filter_map(|v| match v {
                        Value::HashTag(v) => Some(v),
                        Value::String(v) => Some(v.as_str().into()),
                        _ => None,
                    })
                    .collect();
                Value::Set(list.into()).into()
            }
            Expression::Identifier(name) => ctx.get_variable(name),
            Expression::FunctionCall(name, target, args) => {
                let func = ctx
                    .get_function(name)
                    .ok_or_else(|| ExecutionError::UndeclaredReference(name.clone()))?;
                match target {
                    None => {
                        let mut ctx = FunctionContext::new(name.clone(), None, ctx, args.clone());
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
            }
            _ => unimplemented!(),
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
                        .list
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
                    (Value::List(_), index) => Err(ExecutionError::UnsupportedListIndex(index)),
                    (value, index) => Err(ExecutionError::UnsupportedIndex(value, index)),
                }
            }
            Member::Attribute(name) => {
                !unimplemented!()

                // If the property is both an attribute and a method, then we
                // give priority to the property. Maybe we can implement lookahead
                // to see if the next token is a function call?
                /*
                match (child, ctx.has_function(name)) {
                    (None, false) => ExecutionError::NoSuchKey(name.clone()).into(),
                    (Some(child), _) => child.into(),
                    (None, true) => Value::Function(name.clone(), Some(self.into())).into(),
                }
                */
            }
        }
    }

    #[inline(always)]
    fn to_bool(&self) -> bool {
        match self {
            Value::List(v) => !v.list.is_empty(),
            Value::Tuple(v) => !v.list.is_empty(),
            Value::Set(v) => !v.set.is_empty(),
            Value::Number(v) => *v != dec!(0),
            Value::String(v) => !v.is_empty(),
            Value::Bytes(v) => !v.is_empty(),
            Value::Bool(v) => *v,
            Value::Null => false,

            Value::Duration(v) => v.num_nanoseconds().map(|n| n != 0).unwrap_or(false),

            Value::Timestamp(v) => v.timestamp_nanos_opt().unwrap_or_default() > 0,
            Value::Function(_, _) => false,
            Value::Ulid(_) => true,
            Value::HashTag(_) => true,
        }
    }
}

impl From<&Atom> for Value {
    #[inline(always)]
    fn from(atom: &Atom) -> Self {
        match atom {
            Atom::Number(v) => Value::Number(*v),
            Atom::String(v) => Value::String(v.clone()),
            Atom::Bool(v) => Value::Bool(*v),
            Atom::Null => Value::Null,
            Atom::Ulid(v) => Value::Ulid(*v),
            Atom::DateTime(v) => Value::Timestamp(*v),
            Atom::Duration(v) => Value::Duration(*v),
            Atom::HashTag(v) => Value::HashTag(v.clone()),
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
            (Value::List(l), Value::List(r)) => Value::List(
                l.list
                    .iter()
                    .chain(r.list.iter())
                    .cloned()
                    .collect::<Vec<_>>()
                    .into(),
            )
            .into(),
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
    use crate::*;

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
        context.add_variable_from_value("requests", Value::List(requests.into()));
        context.add_variable_from_value("size", Value::Number(3.into()));
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
            Value::List(vec![1.into(), 2.into(), 3.into(), 4.into()].into())
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
        context.add_variable_from_value("list", Value::List(vec![].into()));
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
            Value::List(vec![Value::String("example".to_string())].into())
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

    #[test]
    fn tag_set() {
        let program = Program::compile("{# #1, #2, #3 #}").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(
            result.unwrap(),
            Value::Set(
                vec![
                    Value::HashTag("1".into()),
                    Value::HashTag("2".into()),
                    Value::HashTag("3".into())
                ]
                .into()
            )
        );
    }

    #[test]
    fn tag_set_expression() {
        let program = Program::compile("{# #one, #two, 'thr' + 'ee' #}").unwrap();
        let context = Context::default();
        let result = program.execute(&context);
        assert_eq!(
            result.unwrap(),
            Value::Set(
                vec![
                    Value::HashTag("one".into()),
                    Value::HashTag("two".into()),
                    Value::HashTag("three".into())
                ]
                .into()
            )
        );
    }
}
