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
    pub args: Vec<Expression>,
    pub arg_idx: usize,
}

impl<'context> FunctionContext<'context> {
    pub fn new(
        name: Identifier,
        this: Option<Value>,
        ptx: &'context Context<'context>,
        args: Vec<Expression>,
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
        Value::Tuple(l) => l.list.len(),
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
        Value::Tuple(v) => v.list.contains(&arg),
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
pub fn starts_with(This(this): This<Value>, prefix: Value) -> bool {
    let seek: String = this.into();
    let sought: String = prefix.into();
    seek.starts_with(&sought)
}

/// Returns true if a string ends with another string.
///
/// # Example
/// ```cel
/// "abc".endsWith("c") == true
/// ```
pub fn ends_with(This(this): This<Value>, suffix: Value) -> bool {
    let seek: String = this.into();
    let sought: String = suffix.into();
    seek.ends_with(&sought)
}

/// Returns true if a string matches the regular expression.
///
/// # Example
/// ```cel
/// "abc".matches("^[a-z]*$") == true
/// ```
pub fn matches(ftx: &FunctionContext, This(this): This<Value>, regex: Value) -> Result<bool> {
    let haystack: String = this.into();
    let pattern: String = regex.into();
    match regex::Regex::new(&pattern) {
        Ok(re) => Ok(re.is_match(&haystack)),
        Err(err) => Err(ftx.error(format!("'{pattern}' not a valid regex:\n{err}"))),
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
    match this {
        Value::List(items) => {
            let mut values = Vec::with_capacity(items.list.len());
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.list.iter() {
                ptx.add_variable_from_value(ident.clone(), item.clone());
                let value = ptx.resolve(&expr)?;
                values.push(value);
            }
            Value::List(ValueList { list: values })
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
    match this {
        Value::List(items) => {
            let mut values = Vec::with_capacity(items.list.len());
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.list.iter() {
                ptx.add_variable_from_value(ident.clone(), item.clone());
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    values.push(item.clone());
                }
            }
            Value::List(ValueList { list: values })
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
) -> Result<bool> {
    return match this {
        Value::List(items) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.list.iter() {
                ptx.add_variable_from_value(&ident, item);
                if let Value::Bool(false) = ptx.resolve(&expr)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        _ => return Err(this.error_expected_type(ValueType::List)),
    };
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
) -> Result<bool> {
    match this {
        Value::List(items) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.list.iter() {
                ptx.add_variable_from_value(&ident, item);
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        _ => Err(this.error_expected_type(ValueType::List)),
    }
}

/// Returns a boolean value indicating whether only one value in the provided
/// meets the predicate defined by the provided expression.
///
/// This function is intended to be used like the CEL-go `exists` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].exists_one(x, x > 0) == false
/// [1, 2, 3].exists_one(x, x == 1) == true
/// [{1:true, 2:true, 3:false}].exists_one(x, x > 0) == false
/// ```
pub fn exists_one(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<bool> {
    match this {
        Value::List(items) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            let mut exists = false;
            for item in items.list.iter() {
                ptx.add_variable_from_value(&ident, item);
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    if exists {
                        return Ok(false);
                    }
                    exists = true;
                }
            }
            Ok(exists)
        }
        _ => Err(this.error_expected_type(ValueType::List)),
    }
}

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

pub fn max(Arguments(args): Arguments) -> Result<Value> {
    // If items is a list of values, then operate on the list
    let items = if args.list.len() == 1 {
        match &args.list[0] {
            Value::List(values) => values,
            _ => return Ok(args.list[0].clone()),
        }
    } else {
        &args
    };

    items
        .list
        .iter()
        .skip(1)
        .try_fold(
            items.list.first().unwrap_or(&Value::Null),
            |acc, x| match acc.partial_cmp(x) {
                Some(Ordering::Greater) => Ok(acc),
                Some(_) => Ok(x),
                None => Err(ExecutionError::ValuesNotComparable(acc.clone(), x.clone())),
            },
        )
        .cloned()
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::tests::test_script;

    fn assert_script(input: &(&str, &str)) {
        assert_eq!(test_script(input.1, None), Ok(true.into()), "{}", input.0);
    }

    #[test]
    fn test_has() {
        assert!(false, "TODO: implement entity has");
    }

    #[test]
    fn test_map() {
        [
            ("map list", "[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
            ("map list 2", "[1, 2, 3].map(y, y + 1) == [2, 3, 4]"),
            (
                "nested map",
                "[[1, 2], [2, 3]].map(x, x.map(x, x * 2)) == [[2, 4], [4, 6]]",
            ),
            (
                "map to list",
                r#"{'John': 'smart'}.map(key, key) == ['John']"#,
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_filter() {
        [("filter list", "[1, 2, 3].filter(x, x > 2) == [3]")]
            .iter()
            .for_each(assert_script);
    }

    #[test]
    fn test_all() {
        [
            ("all list #1", "[0, 1, 2].all(x, x >= 0)"),
            ("all list #2", "[0, 1, 2].all(x, x > 0) == false"),
            ("all map", "{0: 0, 1:1, 2:2}.all(x, x >= 0) == true"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_exists() {
        [
            ("exist list #1", "[0, 1, 2].exists(x, x > 0)"),
            ("exist list #2", "[0, 1, 2].exists(x, x == 3) == false"),
            ("exist list #3", "[0, 1, 2, 2].exists(x, x == 2)"),
            ("exist map", "{0: 0, 1:1, 2:2}.exists(x, x > 0)"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_exists_one() {
        [
            ("exist list #1", "[0, 1, 2].exists_one(x, x > 0) == false"),
            ("exist list #2", "[0, 1, 2].exists_one(x, x == 0)"),
            ("exist map", "{0: 0, 1:1, 2:2}.exists_one(x, x == 2)"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_max() {
        [
            ("max single", "max(1) == 1"),
            ("max multiple", "max(1, 2, 3) == 3"),
            ("max negative", "max(-1, 0) == 0"),
            ("max float", "max(-1.0, 0.0) == 0.0"),
            ("max list", "max([1, 2, 3]) == 3"),
            ("max empty list", "max([]) == null"),
            ("max no args", "max() == null"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_starts_with() {
        [
            ("starts with true", "'foobar'.startsWith('foo') == true"),
            ("starts with false", "'foobar'.startsWith('bar') == false"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_ends_with() {
        [
            ("ends with true", "'foobar'.endsWith('bar') == true"),
            ("ends with false", "'foobar'.endsWith('foo') == false"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_timestamp() {
        [
            ("comparison", "2023-05-29T00:00:00Z > 2023-05-28T00:00:00Z"),
            ("comparison", "2023-05-29T00:00:00Z < 2023-05-30T00:00:00Z"),
            (
                "subtracting duration",
                "2023-05-29T00:00:00Z - 24h == 2023-05-28T00:00:00Z",
            ),
            (
                "subtracting date",
                "2023-05-29T00:00:00Z - 2023-05-28T00:00:00Z == 24h",
            ),
            (
                "adding duration",
                "2023-05-28T00:00:00Z + 24h == 2023-05-29T00:00:00Z",
            ),
            (
                "timestamp string",
                "2023-05-28T00:00:00Z.string() == '2023-05-28T00:00:00+00:00'",
            ),
            (
                "timestamp getFullYear",
                "2023-05-28T00:00:00Z.getFullYear() == 2023",
            ),
            ("timestamp getMonth", "2023-05-28T00:00:00Z.getMonth() == 4"),
            (
                "timestamp getDayOfMonth",
                "2023-05-28T00:00:00Z.getDayOfMonth() == 27",
            ),
            (
                "timestamp getDayOfYear",
                "2023-05-28T00:00:00Z.getDayOfYear() == 147",
            ),
            ("timestamp getDate", "2023-05-28T00:00:00Z.getDate() == 28"),
            (
                "timestamp getDayOfWeek",
                "2023-05-28T00:00:00Z.getDayOfWeek() == 0",
            ),
            ("timestamp getHours", "2023-05-28T02:00:00Z.getHours() == 2"),
            (
                "timestamp getMinutes",
                " 2023-05-28T00:05:00Z.getMinutes() == 5",
            ),
            (
                "timestamp getSeconds",
                "2023-05-28T00:00:06Z.getSeconds() == 6",
            ),
            (
                "timestamp getMilliseconds",
                "2023-05-28T00:00:42.123Z.getMilliseconds() == 123",
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_timestamp_variable() {
        let mut context = Context::default();
        let ts: chrono::DateTime<chrono::Utc> =
            chrono::DateTime::parse_from_rfc3339("2023-05-29T00:00:00Z")
                .map(|e| e.with_timezone(&chrono::Utc))
                .unwrap();
        context.add_variable_from_value("ts", crate::Value::Timestamp(ts));

        let program = crate::Program::compile("ts == 2023-05-29T00:00:00Z").unwrap();
        let result = program.execute(&context).unwrap();
        assert_eq!(result, true.into());
    }

    #[cfg(feature = "regex")]
    #[test]
    fn test_matches() {
        let tests = vec![
            ("string", "'foobar'.matches('^[a-zA-Z]*$') == true"),
            (
                "map",
                "{'1': 'abc', '2': 'def', '3': 'ghi'}.all(key, key.matches('^[a-zA-Z]*$')) == false",
            ),
        ];

        for (name, script) in tests {
            assert_eq!(
                test_script(script, None),
                Ok(true.into()),
                ".matches failed for '{name}'"
            );
        }
    }

    #[cfg(feature = "regex")]
    #[test]
    fn test_matches_err() {
        assert_eq!(
            test_script(
                "'foobar'.matches('(foo') == true", None),
            Err(
                crate::ExecutionError::FunctionError {
                    function: "matches".into(),
                    message: "'(foo' not a valid regex:\nregex parse error:\n    (foo\n    ^\nerror: unclosed group".to_string()
                }
            )
        );
    }
}
