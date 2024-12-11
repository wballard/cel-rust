use crate::context::Context;
use crate::magic::{Arguments, This};
use crate::objects::{Value, ValueList, ValueType};
use crate::resolvers::{Argument, Resolver};
use crate::ExecutionError;
use cel_parser::*;
use std::cmp::Ordering;

type Result<T> = std::result::Result<T, ExecutionError>;

/// `FunctionContext` is a context object passed to functions when they are called.
///
/// It contains references to the target object (if the function is called as
/// a method), the program context ([`Context`]) which gives functions access
/// to variables, and the arguments to the function call.
#[derive(Clone)]
pub struct FunctionContext<'context> {
    pub name: Identifier,
    pub this: Option<Value>,
    pub ptx: &'context Context<'context>,
    pub args: Vec<&'context Expression>,
    pub arg_idx: usize,
}

impl<'context> FunctionContext<'context> {
    pub fn new(
        name: Identifier,
        this: Option<Value>,
        ptx: &'context Context<'context>,
        args: Vec<&'context Expression>,
    ) -> Self {
        Self {
            name,
            this,
            ptx,
            args,
            arg_idx: 0,
        }
    }

    /// Resolves the given expression using the program's [`Context`].
    pub fn resolve<R>(&self, resolver: R) -> Result<Value>
    where
        R: Resolver,
    {
        resolver.resolve(self)
    }

    /// Returns an execution error for the currently execution function.
    pub fn error<M: ToString>(&self, message: M) -> ExecutionError {
        ExecutionError::function_error(self.name.clone(), message)
    }
}

/// Calculates the size of either the target, or the provided args depending on how
/// the function is called.
///
/// If called as a method, the target will be used. If called as a function, the
/// first argument will be used.
///
/// The following [`Value`] variants are supported:
/// * [`Value::List`]
/// * [`Value::Map`]
/// * [`Value::String`]
/// * [`Value::Bytes`]
///
/// # Examples
/// ```skip
/// size([1, 2, 3]) == 3
/// ```
/// ```skip
/// 'foobar'.size() == 6
/// ```
pub fn size(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    let size = match this {
        Value::List(l) => l.list.len(),
        Value::Set(t) => t.set.len(),
        Value::String(s) => s.len(),
        Value::HashTag(h) => h.as_ref().len(),
        Value::Number(_) => 1,
        value => return Err(ftx.error(format!("cannot determine the size of {:?}", value))),
    };
    Ok(Value::Number(size.into()))
}

/// Returns true if the target contains the provided argument. The actual behavior
/// depends mainly on the type of the target.
///
/// The following [`Value`] variants are supported:
/// * [`Value::List`] - Returns true if the list contains the provided value.
/// * [`Value::Map`] - Returns true if the map contains the provided key.
/// * [`Value::String`] - Returns true if the string contains the provided substring.
/// * [`Value::Bytes`] - Returns true if the bytes contain the provided byte.
///
/// # Example
///
/// ## List
/// ```cel
/// [1, 2, 3].contains(1) == true
/// ```
///
/// ## Map
/// ```cel
/// {"a": 1, "b": 2, "c": 3}.contains("a") == true
/// ```
///
/// ## String
/// ```cel
/// "abc".contains("b") == true
/// ```
///
/// ## Bytes
/// ```cel
/// b"abc".contains(b"c") == true
/// ```
pub fn contains(This(this): This<Value>, arg: Value) -> Result<Value> {
    Ok(match this {
        Value::List(v) => v.list.contains(&arg),
        Value::Set(v) => v.set.contains(&arg),
        Value::String(s) => {
            if let Value::String(arg) = arg {
                s.contains(arg.as_str())
            } else {
                false
            }
        }
        _ => false,
    }
    .into())
}

// Performs a type conversion on the target.
pub fn string(_: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    // this is not a 'display string' -- it's an actual string
    let s: String = this.into();
    Ok(s.into())
}

/// Returns true if a string starts with another string.
///
/// # Example
/// ```cel
/// "abc".startsWith("a") == true
/// ```
pub fn starts_with(This(this): This<Value>, prefix: Value) -> Result<Value> {
    let v = match this {
        Value::String(s) => {
            let seek: String = s;
            let sought: String = prefix.into();
            seek.starts_with(&sought)
        }
        Value::HashTag(h) => {
            let seek: String = h.as_ref().into();
            let sought: String = prefix.into();
            seek.starts_with(&sought)
        }
        Value::List(l) => match l.list.first() {
            Some(v) => v == &prefix,
            _ => false,
        },
        _ => false,
    };
    Ok(v.into())
}

/// Returns true if a string ends with another string.
///
/// # Example
/// ```cel
/// "abc".endsWith("c") == true
/// ```
pub fn ends_with(This(this): This<Value>, suffix: Value) -> Result<Value> {
    let v = match this {
        Value::String(s) => {
            let seek: String = s;
            let sought: String = suffix.into();
            seek.ends_with(&sought)
        }
        Value::HashTag(h) => {
            let seek: String = h.as_ref().into();
            let sought: String = suffix.into();
            seek.ends_with(&sought)
        }
        Value::List(l) => match l.list.last() {
            Some(v) => v == &suffix,
            _ => false,
        },
        _ => false,
    };
    Ok(v.into())
}

/// Returns true if a string matches the regular expression.
///
/// # Example
/// ```cel
/// "abc".matches("^[a-z]*$") == true
/// ```
pub fn matches(ftx: &FunctionContext, This(this): This<Value>, regex: Value) -> Result<bool> {
    let haystack: String = this.into();
    match regex {
        Value::Regex(re) => Ok(re.0.is_match(&haystack)),
        _ => {
            let pattern: String = regex.into();
            match regex::Regex::new(&pattern) {
                Ok(re) => Ok(re.is_match(&haystack)),
                Err(err) => Err(ftx.error(format!("'{pattern}' not a valid regex:\n{err}"))),
            }
        }
    }
}

/// Returns true if the provided argument can be resolved.
///
/// This function is useful for checking if a property exists on a type before
/// attempting to resolve it. Resolving a property that does not exist will
/// result in a [`ExecutionError::NoSuchKey`] error.
///
/// Operates similar to the `has` macro describe in the Go CEL implementation
/// spec: <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>.
///
/// # Examples
/// ```cel
/// has(foo.bar.baz)
/// ```
pub fn has(ftx: &FunctionContext) -> Result<Value> {
    // We determine if a type has a property by attempting to resolve it.
    // If we get a NoSuchKey error, then we know the property does not exist
    match ftx.resolve(Argument(0)) {
        Ok(_) => Value::Bool(true),
        Err(err) => match err {
            ExecutionError::NoSuchKey(_) => Value::Bool(false),
            _ => return Err(err),
        },
    }
    .into()
}
// TODO: test with slice

/// Maps the provided list to a new list by applying an expression to each
/// input item.
///
/// This function is intended to be used like the CEL-go `map` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Examples
/// ```cel
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
/// ```
pub fn map(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<Value> {
    fn _apply<'a, T: Iterator<Item = &'a Value>>(
        vals: T,
        len: usize,
        ftx: &FunctionContext,
        ident: &Identifier,
        expr: &Expression,
    ) -> Result<Vec<Value>> {
        let mut values = Vec::with_capacity(len);
        let mut ptx = ftx.ptx.new_inner_scope();
        for item in vals {
            ptx.add_variable_from_value(ident.clone(), item.clone());
            let value = ptx.resolve(expr)?;
            values.push(value);
        }
        Ok(values)
    }
    match this {
        Value::List(items) => Value::List(ValueList {
            list: _apply(items.list.iter(), items.list.len(), ftx, &ident, &expr)?,
        }),
        Value::Set(items) => {
            Value::Set(_apply(items.set.iter(), items.set.len(), ftx, &ident, &expr)?.into())
        }
        _ => return Err(this.error_expected_type(ValueType::List)),
    }
    .into()
}

/// Filters the provided list by applying an expression to each input item
/// and including the input item in the resulting, only if the expression
/// returned true.
///
/// This function is intended to be used like the CEL-go `filter` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].filter(x, x > 1) == [2, 3]
/// ```
pub fn filter(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<Value> {
    fn _apply<'a, T: Iterator<Item = &'a Value>>(
        vals: T,
        len: usize,
        ftx: &FunctionContext,
        ident: &Identifier,
        expr: &Expression,
    ) -> Result<Vec<Value>> {
        let mut values = Vec::with_capacity(len);
        let mut ptx = ftx.ptx.new_inner_scope();
        for item in vals {
            ptx.add_variable_from_value(ident.clone(), item.clone());
            let value = ptx.resolve(expr)?;
            if value.to_bool() {
                values.push(item.clone());
            }
        }
        Ok(values)
    }

    match this {
        Value::List(items) => Value::List(ValueList {
            list: _apply(items.list.iter(), items.list.len(), ftx, &ident, &expr)?,
        }),
        Value::Set(items) => {
            Value::Set(_apply(items.set.iter(), items.set.len(), ftx, &ident, &expr)?.into())
        }
        _ => return Err(this.error_expected_type(ValueType::List)),
    }
    .into()
}

/// Returns a boolean value indicating whether every value in the provided
/// meet the predicate defined by the provided expression. If
/// called on a map, the predicate is applied to the map keys.
///
/// This function is intended to be used like the CEL-go `all` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].all(x, x > 0) == true
/// ```
pub fn all(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<Value> {
    fn _apply<'a, T: Iterator<Item = &'a Value>>(
        vals: T,
        ftx: &FunctionContext,
        ident: &Identifier,
        expr: &Expression,
    ) -> Result<Value> {
        let mut ptx = ftx.ptx.new_inner_scope();
        for item in vals {
            ptx.add_variable_from_value(ident.clone(), item.clone());
            let value = ptx.resolve(expr)?;
            if value.to_bool() {
                // keep on going
            } else {
                return Ok(false.into());
            }
        }
        Ok(true.into())
    }
    match this {
        Value::List(items) => _apply(items.list.iter(), ftx, &ident, &expr)?,
        Value::Set(items) => _apply(items.set.iter(), ftx, &ident, &expr)?,
        _ => return Err(this.error_expected_type(ValueType::List)),
    }
    .into()
}

/// Returns a boolean value indicating whether a or more values in the provided
/// meet the predicate defined by the provided expression.
///
/// If called on a map, the predicate is applied to the map keys.
///
/// This function is intended to be used like the CEL-go `exists` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].exists(x, x > 0) == true
/// ```
pub fn exists(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<Value> {
    let empty = filter(ftx, This(this), ident, expr).map(|v| !v.is_empty());
    match empty {
        Ok(v) => Ok(v.into()),
        Err(e) => Err(e),
    }
}

// TODO: exist_one
// TODO: alias exist, exist_one

pub mod time {
    use super::Result;
    use crate::magic::This;
    use crate::Value;
    use chrono::{Datelike, Days, Months, Timelike};

    pub fn timestamp_year(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok(this.year().into())
    }

    pub fn timestamp_month(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.month0() as i32).into())
    }

    pub fn timestamp_year_day(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        let year = this
            .checked_sub_days(Days::new(this.day0() as u64))
            .unwrap()
            .checked_sub_months(Months::new(this.month0()))
            .unwrap();
        Ok(this.signed_duration_since(year).num_days().into())
    }

    pub fn timestamp_month_day(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.day0() as i32).into())
    }

    pub fn timestamp_date(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.day() as i32).into())
    }

    pub fn timestamp_weekday(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.weekday().num_days_from_sunday() as i32).into())
    }

    pub fn timestamp_hours(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.hour() as i32).into())
    }

    pub fn timestamp_minutes(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.minute() as i32).into())
    }

    pub fn timestamp_seconds(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.second() as i32).into())
    }

    pub fn timestamp_millis(This(this): This<chrono::DateTime<chrono::Utc>>) -> Result<Value> {
        Ok((this.timestamp_subsec_millis() as i32).into())
    }
}

pub fn max(ftx: &FunctionContext, Arguments(args): Arguments) -> Result<Value> {
    fn _fold<'a>(acc: &'a Value, x: &'a Value) -> Result<&'a Value> {
        match acc.partial_cmp(x) {
            Some(Ordering::Greater) => Ok(acc),
            Some(_) => Ok(x),
            None => Err(ExecutionError::ValuesNotComparable(acc.clone(), x.clone())),
        }
    }
    // If items is multiple values, then operate on it directly
    let refret = match &ftx.this {
        Some(this) => match this {
            Value::List(values) => values.list.iter().try_fold(&Value::Null, _fold),
            Value::Set(values) => values.set.iter().try_fold(&Value::Null, _fold),
            _ => Err(ExecutionError::NotSupportedAsMethod {
                method: "max".to_string(),
                target: this.clone(),
            })?,
        },
        None => {
            if args.list.len() == 1 {
                // if we only have one multi valued item, call max on it
                match &(args.list[0]) {
                    Value::List(values) => values.list.iter().try_fold(&Value::Null, _fold),
                    Value::Set(values) => values.set.iter().try_fold(&Value::Null, _fold),
                    _ => return Ok(args.list[0].clone()),
                }
            } else {
                args.list.iter().try_fold(&Value::Null, _fold)
            }
        }
    };

    match refret {
        Ok(v) => Ok(v.clone()),
        Err(e) => Err(e),
    }
}

// TODO: min
