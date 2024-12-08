use crate::macros::impl_conversions;
use crate::resolvers::{AllArguments, Argument};
use crate::{ExecutionError, FunctionContext, ResolveResult, Value, ValueList};
use cel_parser::*;

impl_conversions!(
    rust_decimal::Decimal => Value::Number,
    String => Value::String,
    bool => Value::Bool,
    ValueList => Value::List
);

impl_conversions!(
    chrono::Duration => Value::Duration,
    chrono::DateTime<chrono::Utc> => Value::Timestamp,
);

/// Describes any type that can be converted from a [`Value`] into itself.
/// This is commonly used to convert from [`Value`] into primitive types,
/// e.g. from `Value::Bool(true) -> true`. This trait is auto-implemented
/// for many CEL-primitive types.
trait FromValue {
    fn from_value(value: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

impl FromValue for Value {
    fn from_value(value: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        Ok(value.clone())
    }
}

/// A trait for types that can be converted into a [`ResolveResult`]. Every function that can
/// be registered to the CEL context must return a value that implements this trait.
pub trait IntoResolveResult {
    fn into_resolve_result(self) -> ResolveResult;
}

impl IntoResolveResult for Result<Value, ExecutionError> {
    fn into_resolve_result(self) -> ResolveResult {
        self
    }
}

/// Describes any type that can be converted from a [`FunctionContext`] into
/// itself, for example CEL primitives implement this trait to allow them to
/// be used as arguments to functions. This trait is core to the 'magic function
/// parameter' system. Every argument to a function that can be registered to
/// the CEL context must implement this type.
pub(crate) trait FromContext<'a, 'context> {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

/// A function argument abstraction enabling dynamic method invocation on a
/// target instance or on the first argument if the function is not called
/// as a method.
///
/// This is similar to how methods can be called as functions using the
/// [fully-qualified syntax](https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#fully-qualified-syntax-for-disambiguation-calling-methods-with-the-same-name).
pub struct This<T>(pub T);

impl<'a, 'context, T> FromContext<'a, 'context> for This<T>
where
    T: FromValue,
{
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        if let Some(ref this) = ctx.this {
            Ok(This(T::from_value(this)?))
        } else {
            let arg = arg_value_from_context(ctx)
                .map_err(|_| ExecutionError::missing_argument_or_target())?;
            Ok(This(T::from_value(&arg)?))
        }
    }
}

/// Identifier is an argument extractor that attempts to extract an identifier
/// from an argument's expression.
///
/// It fails if the argument is not available, or if the argument cannot be
/// converted into an expression.
///
/// # Examples
/// Identifiers are useful for functions like `.map` or `.filter` where one
/// of the arguments is the declaration of a variable. In this case, as noted
/// below, the x is an identifier, and we want to be able to parse it
/// automatically.
///
/// ```javascript
/// //        Identifier
/// //            ↓
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
/// ```
///
/// The function signature for the Rust implementation of `map` looks like this
///
/// ```skip
/// pub fn map(
///     ftx: &FunctionContext,
///     This(this): This<Value>, // <- [1, 2, 3]
///     ident: Identifier,       // <- x
///     expr: Expression,        // <- x * 2
/// ) -> Result<Value>;
/// ```
impl<'a, 'context> FromContext<'a, 'context> for Identifier {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        match arg_expr_from_context(ctx) {
            Expression::Identifier(ident) => Ok(ident.clone()),
            expr => Err(ExecutionError::UnexpectedType {
                got: format!("{:?}", expr),
                want: "identifier".to_string(),
            }),
        }
    }
}

/// An argument extractor that extracts all the arguments passed to a function, resolves their
/// expressions and returns a vector of [`Value`].
///
/// This is useful for functions that accept a variable number of arguments rather than known
/// arguments and types (for example a `sum` function).
#[derive(Clone)]
pub struct Arguments(pub ValueList);

impl<'a, 'context> FromContext<'a, 'context> for Arguments {
    fn from_context(ctx: &'a mut FunctionContext) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        match ctx.resolve(AllArguments)? {
            Value::List(list) => Ok(Arguments(list.clone())),
            _ => todo!(),
        }
    }
}

impl<'a, 'context> FromContext<'a, 'context> for Value {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        arg_value_from_context(ctx)
    }
}

impl<'a, 'context> FromContext<'a, 'context> for Expression {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        Ok(arg_expr_from_context(ctx))
    }
}

/// Returns the next argument specified by the context's `arg_idx` field as an expression
/// (i.e. not resolved). Calling this multiple times will increment the `arg_idx` which will
/// return subsequent arguments every time.
///
/// Calling this function when there are no more arguments will result in a panic. Since this
/// function is only ever called within the context of a controlled macro that calls it once
/// for each argument, this should never happen.
fn arg_expr_from_context(ctx: &mut FunctionContext) -> Expression {
    let idx = ctx.arg_idx;
    ctx.arg_idx += 1;
    ctx.args[idx].clone()
}

/// Returns the next argument specified by the context's `arg_idx` field as after resolving
/// it. Calling this multiple times will increment the `arg_idx` which will return subsequent
/// arguments every time.
///
/// Calling this function when there are no more arguments will result in a panic. Since this
/// function is only ever called within the context of a controlled macro that calls it once
/// for each argument, this should never happen.
fn arg_value_from_context(ctx: &mut FunctionContext) -> Result<Value, ExecutionError> {
    let idx = ctx.arg_idx;
    ctx.arg_idx += 1;
    ctx.resolve(Argument(idx))
}
