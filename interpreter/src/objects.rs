use crate::context::Context;
use crate::Atom;
use crate::ExecutionError;
use crate::FunctionContext;
use cel_parser::*;
use chrono::SecondsFormat;
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};

/// Holds a list of values.
#[derive(Debug, PartialEq, Clone, Hash, PartialOrd, Ord, Eq)]
pub struct ValueList {
    pub list: Vec<Value>,
}

impl ValueList {
    fn empty() -> Self {
        ValueList { list: vec![] }
    }
}

impl From<Vec<Value>> for ValueList {
    fn from(list: Vec<Value>) -> Self {
        ValueList { list }
    }
}

impl From<Vec<&Value>> for ValueList {
    fn from(list: Vec<&Value>) -> Self {
        let list = list.into_iter().map(|v| v.clone()).collect();
        ValueList { list }
    }
}

/// Holds a set of values.
///
/// This is ideally used for HashTags and Identifiers.
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

impl From<ValueSet> for ValueList {
    fn from(set: ValueSet) -> Self {
        ValueList {
            list: set.set.into_iter().collect(),
        }
    }
}

/// Values that can be computed by the interpreter.
#[derive(Debug, Clone)]
pub enum Value {
    // structures
    List(ValueList),
    Tuple(ValueList),
    Set(ValueSet),
    Function(Identifier, Option<Box<Value>>),
    Range(Box<Value>, Box<Value>),
    // Atoms
    Number(Decimal),
    String(String),
    Bool(bool),
    Duration(chrono::Duration),
    Timestamp(chrono::DateTime<chrono::Utc>),
    Null,
    Ulid(ulid::Ulid),
    HashTag(HashTag),
}

impl Value {
    pub fn len(&self) -> usize {
        match self {
            Value::List(v) => v.list.len(),
            Value::Tuple(v) => v.list.len(),
            Value::Set(v) => v.set.len(),
            Value::String(v) => v.len(),
            Value::HashTag(v) => v.len(),
            // other values are canonically 1
            _ => 1,
        }
    }
    pub fn is_empty(&self) -> bool {
        match self {
            Value::List(v) => v.list.is_empty(),
            Value::Tuple(v) => v.list.is_empty(),
            Value::Set(v) => v.set.is_empty(),
            Value::String(v) => v.is_empty(),
            Value::HashTag(v) => v.is_empty(),
            // other values are canonically not empty
            _ => false,
        }
    }
}

impl TryFrom<Value> for Decimal {
    type Error = ExecutionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(v) => Ok(v),
            Value::String(v) => v.parse().map_err(|_| ExecutionError::UnexpectedType {
                got: v,
                want: ValueType::Number.to_string(),
            }),
            _ => Err(ExecutionError::UnexpectedType {
                got: value.type_of().to_string(),
                want: ValueType::Number.to_string(),
            }),
        }
    }
}

impl TryFrom<&Value> for usize {
    type Error = ExecutionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(v) => match v.to_usize() {
                Some(v) => Ok(v),
                None => Err(ExecutionError::UnexpectedType {
                    got: v.to_string(),
                    want: ValueType::Number.to_string(),
                }),
            },
            _ => Err(ExecutionError::UnexpectedType {
                got: value.type_of().to_string(),
                want: ValueType::Number.to_string(),
            }),
        }
    }
}

/// Reverse type mapping to allow type inspection.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    Range,
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
            ValueType::Range => write!(f, "range"),
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
            Value::Bool(_) => ValueType::Bool,
            Value::Duration(_) => ValueType::Duration,
            Value::Timestamp(_) => ValueType::Timestamp,
            Value::Null => ValueType::Null,
            Value::Ulid(_) => ValueType::Ulid,
            Value::HashTag(_) => ValueType::Tag,
            Value::Set(_) => ValueType::TagSet,
            Value::Range(_, _) => ValueType::Range,
        }
    }

    pub fn error_expected_type(&self, expected: ValueType) -> ExecutionError {
        ExecutionError::UnexpectedType {
            got: self.type_of().to_string(),
            want: expected.to_string(),
        }
    }
}

fn delimit_display<'a, T: Iterator<Item = &'a Value>>(
    vals: T,
    open: char,
    close: char,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    write!(f, "{}", open)?;
    for (i, v) in vals.enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{}", v)?;
    }
    write!(f, "{}", close)
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(v) => delimit_display(v.list.iter(), '[', ']', f),
            Value::Tuple(v) => delimit_display(v.list.iter(), '(', ')', f),
            Value::Set(v) => delimit_display(v.set.iter(), '{', '}', f),
            Value::Function(name, target) => match target {
                Some(target) => write!(f, "{}.{}", target, name),
                None => write!(f, "{}", name),
            },
            Value::Number(v) => write!(f, "{}", v),
            // show strings in quotes to make them easier to read
            // and make sure to escape them -- this allows round-tripping
            Value::String(v) => write!(f, "'{}'", v.replace("'", "\\'")),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Null => write!(f, "null"),
            Value::Duration(v) => write!(f, "{}s", v.num_seconds()),
            Value::Timestamp(v) => write!(f, "{}", v.to_rfc3339_opts(SecondsFormat::Millis, true)),
            Value::Ulid(v) => write!(f, "&{}", v),
            Value::HashTag(v) => write!(f, "{}", v),
            Value::Range(low, high) => write!(f, "{}..{}", low, high),
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

impl From<Decimal> for Value {
    fn from(value: Decimal) -> Self {
        Value::Number(value)
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

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Value::String(v.to_string())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Function(a1, a2), Value::Function(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
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
            Value::Bool(v) => v.hash(state),
            Value::Null => Value::Null.hash(state),

            Value::Duration(v) => v.hash(state),

            Value::Timestamp(v) => v.hash(state),
            Value::Ulid(v) => v.hash(state),
            Value::HashTag(v) => v.hash(state),
            Value::Set(v) => v.hash(state),
            Value::Range(low, high) => {
                low.hash(state);
                high.hash(state);
            }
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
            // any value is greater than null -- this allows fold and reduce to work
            (Value::Null, _) => Ordering::Less,
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

fn delimit<'a, T: Iterator<Item = &'a Value>>(vals: T, open: char, close: char) -> String {
    let mut buf = String::new();
    buf.push(open);
    for (i, v) in vals.enumerate() {
        if i > 0 {
            buf.push_str(", ");
        }
        // using the `into` conversion to get the unquoted version
        let s: String = v.clone().into();
        buf.push_str(s.as_str())
    }
    buf.push(close);
    buf
}

/// These are the `into` versions -- without quotes and escapes.
impl From<Value> for String {
    fn from(value: Value) -> Self {
        match value {
            Value::String(v) => v.clone(),
            Value::Bool(v) => format!("{}", v),
            Value::Number(v) => format!("{}", v),
            Value::Null => "".to_string(),
            Value::Duration(v) => format!("{}s", v.num_seconds()),
            Value::Timestamp(v) => v.to_rfc3339(),
            Value::Ulid(v) => format!("&{}", v),
            Value::List(v) => delimit(v.list.iter(), '[', ']'),
            Value::Tuple(v) => delimit(v.list.iter(), '(', ')'),
            Value::Set(v) => delimit(v.set.iter(), '{', '}'),
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
            Expression::Unary(op, right) => {
                let func = ctx
                    .get_operator(op)
                    .ok_or_else(|| ExecutionError::UndefinedOperator(op.clone()))?;
                let mut ctx = FunctionContext::new(
                    Identifier(op.to_string()),
                    None,
                    ctx,
                    right.to_arguments(),
                );
                func.call_with_context(&mut ctx)
            }
            Expression::Binary(left, op, right) => {
                let func = ctx
                    .get_operator(op)
                    .ok_or_else(|| ExecutionError::UndefinedOperator(op.clone()))?;
                let arguments = vec![&**left, &**right];
                let mut ctx =
                    FunctionContext::new(Identifier(op.to_string()), None, ctx, arguments);
                func.call_with_context(&mut ctx)
            }
            Expression::Ternary(cond, left, right) => {
                let cond = Value::resolve(cond, ctx)?;
                if cond.to_bool() {
                    Value::resolve(left, ctx)
                } else {
                    Value::resolve(right, ctx)
                }
            }
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
                let list = items
                    .iter()
                    .map(|i| Value::resolve(i, ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Value::Set(list.into()).into()
            }
            Expression::Identifier(name) => ctx.get_variable(name),
            Expression::FunctionCall(function, arguments) => {
                match &**function {
                    // this looks a little weird, but it's because we need to handle
                    // 1-1 where there is no space and the tokens are 1 and -1 with no operator
                    Expression::Atom(Atom::Number(lhs)) => match &**arguments {
                        Expression::Atom(Atom::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
                        expr => {
                            let target = Value::resolve(expr, ctx)?;
                            target.error_expected_type(ValueType::Number).into()
                        }
                    },
                    Expression::Identifier(name) => {
                        let func = ctx
                            .get_function(name)
                            .ok_or_else(|| ExecutionError::UndeclaredReference(name.clone()))?;
                        let mut ctx =
                            FunctionContext::new(name.clone(), None, ctx, arguments.to_arguments());
                        func.call_with_context(&mut ctx)
                    }
                    Expression::Binary(
                        target,
                        Operator::Relation(RelationOp::GetMember),
                        function_name,
                    ) => match &**function_name {
                        Expression::Identifier(name) => {
                            let target = Value::resolve(target, ctx)?;
                            let func = ctx
                                .get_function(name)
                                .ok_or_else(|| ExecutionError::UndeclaredReference(name.clone()))?;
                            let mut ctx = FunctionContext::new(
                                name.clone(),
                                Some(target),
                                ctx,
                                arguments.to_arguments(),
                            );
                            func.call_with_context(&mut ctx)
                        }
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }
            Expression::Indexer(target, indexes) => {
                let target = Value::resolve(target, ctx)?;
                let indexes = Value::resolve(indexes, ctx)?;
                target.member_by_indexer(ctx, &indexes)
            }
            Expression::Empty => Ok(Value::Set(vec![].into())),
            Expression::Multiple(exprs) => {
                let mut res = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    res.push(Value::resolve(expr, ctx)?);
                }
                Ok(Value::List(res.into()))
            }
        }
    }

    fn member_by_indexer(&self, ctx: &Context, indexer: &Value) -> ResolveResult {
        match indexer {
            Value::Range(low, high) => match (&**low, &**high) {
                (Value::Number(low), Value::Number(high)) => {
                    let low = low
                        .to_usize()
                        .ok_or(ExecutionError::UnsupportedListIndex(indexer.clone()))?;
                    let high = high
                        .to_usize()
                        .ok_or(ExecutionError::UnsupportedListIndex(indexer.clone()))?;
                    match self {
                        // adding one here -- we have inclusive ranges
                        Value::List(items) => {
                            let list = items.list.iter().skip(low).take(high - low + 1);
                            Ok(Value::List(list.cloned().collect::<Vec<_>>().into()))
                        }
                        Value::String(str) => match str.get(low..(high + 1)) {
                            None => Ok(Value::String("".to_string())),
                            Some(str) => Ok(Value::String(str.to_string())),
                        },
                        _ => Err(ExecutionError::UnsupportedListIndex(indexer.clone())),
                    }
                }
                _ => Err(ExecutionError::UnsupportedListIndex(indexer.clone())),
            },
            Value::List(indexes) => match self {
                Value::List(items) => match indexes.list.len() {
                    0 => Ok(Value::List(ValueList::empty())),
                    1 => match indexes.list.first() {
                        Some(idx) => self.member_by_indexer(ctx, idx),
                        _ => Err(ExecutionError::UnsupportedListIndex(indexer.clone())),
                    },
                    _ => {
                        let indexes = indexes
                            .list
                            .iter()
                            .flat_map(|v| v.try_into())
                            .collect::<Vec<usize>>();
                        let values = indexes
                            .iter()
                            .flat_map(|idx| items.list.get(*idx))
                            .collect::<Vec<&Value>>();
                        Ok(Value::List(values.into()))
                    }
                },
                Value::String(v) => match indexes.list.len() {
                    0 => Ok(Value::String("".to_string())),
                    1 => match indexes.list.first() {
                        Some(idx) => self.member_by_indexer(ctx, idx),
                        _ => Err(ExecutionError::UnsupportedListIndex(indexer.clone())),
                    },
                    _ => {
                        let indexes = indexes
                            .list
                            .iter()
                            .flat_map(|v| v.try_into())
                            .collect::<Vec<usize>>();
                        let values = indexes
                            .iter()
                            .flat_map(|idx| v.get(*idx..(*idx + 1)))
                            .collect::<String>();

                        Ok(Value::String(values))
                    }
                },
                _ => Err(ExecutionError::UnsupportedIndex(
                    self.clone(),
                    indexer.clone(),
                )),
            },
            Value::Number(idx) => {
                let idx = idx
                    .to_usize()
                    .ok_or(ExecutionError::UnsupportedListIndex(indexer.clone()))?;
                match self {
                    Value::List(items) => {
                        let item = items.list.get(idx);
                        match item {
                            Some(item) => Ok(item.into()),
                            None => Ok(Value::List(ValueList::empty())),
                        }
                    }
                    Value::String(v) => match v.get(idx..(idx + 1)) {
                        Some(str) => Ok(Value::String(str.to_string())),
                        None => Ok(Value::String("".to_string())),
                    },
                    _ => Err(ExecutionError::UnsupportedIndex(
                        self.clone(),
                        indexer.clone(),
                    )),
                }
            }
            index => Err(ExecutionError::UnsupportedIndex(
                self.clone(),
                index.clone(),
            )),
        }
    }

    #[inline(always)]
    pub fn to_bool(&self) -> bool {
        match self {
            Value::List(v) => !v.list.is_empty(),
            Value::Tuple(v) => !v.list.is_empty(),
            Value::Set(v) => !v.set.is_empty(),
            Value::Number(v) => *v != dec!(0),
            Value::String(v) => !v.is_empty(),
            Value::Bool(v) => *v,
            Value::Null => false,
            Value::Duration(v) => v.num_nanoseconds().map(|n| n != 0).unwrap_or(false),
            Value::Timestamp(v) => v.timestamp_nanos_opt().unwrap_or_default() > 0,
            Value::Function(_, _) => true,
            Value::Ulid(_) => true,
            Value::HashTag(_) => true,
            Value::Range(_, _) => true,
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

#[cfg(test)]
mod tests {
    use crate::*;

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
                Operator::Arithmetic(ArithmeticOp::Subtract),
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
                Operator::Arithmetic(ArithmeticOp::Divide),
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
                Operator::Arithmetic(ArithmeticOp::Modulus),
                "foo".into(),
                Value::Number(10.into()),
            ),
        );
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
}
