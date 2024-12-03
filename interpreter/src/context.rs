use crate::function_registry::{Function, FunctionRegistry, Handler};
use crate::objects::Value;
use crate::operators::*;
use crate::{functions, ExecutionError};
use cel_parser::*;
use std::collections::HashMap;

/// Context is a collection of variables and functions that can be used
/// by the interpreter to resolve expressions.
///
/// The context can be either a parent context, or a child context. A
/// parent context is created by default and contains all of the built-in
/// functions.
///
/// A child context has it's own variables (which can be added to), but it
/// will also reference the parent context. This allows for variables to
/// be overridden within the child context while still being able to
/// resolve variables in the child's parents.
///
/// So why is this important? Well some CEL-macros such as the `.map` macro
/// declare intermediate user-specified identifiers that should only be
/// available within the macro, and should not override variables in the
/// parent context. The `.map` macro can clone the parent context, add the
/// intermediate identifier to the child context, and then evaluate the
/// map expression.
///
/// Intermediate variable stored in child context
///               ↓
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
///                  ↑
/// Only in scope for the duration of the map expression
///
pub enum Context<'a> {
    Root {
        functions: FunctionRegistry<Identifier>,
        operators: FunctionRegistry<Operator>,
        variables: HashMap<Identifier, Value>,
    },
    Child {
        parent: &'a Context<'a>,
        variables: HashMap<Identifier, Value>,
    },
}

impl<'a> Context<'a> {
    pub fn add_variable_from_value<S, V>(&mut self, name: S, value: V)
    where
        S: Into<Identifier>,
        V: Into<Value>,
    {
        match self {
            Context::Root { variables, .. } => {
                variables.insert(name.into(), value.into());
            }
            Context::Child { variables, .. } => {
                variables.insert(name.into(), value.into());
            }
        }
    }

    pub fn get_variable(&self, name: &Identifier) -> Result<Value, ExecutionError> {
        match self {
            Context::Child { variables, parent } => variables
                .get(name)
                .cloned()
                .or_else(|| parent.get_variable(name).ok())
                .ok_or_else(|| ExecutionError::UndeclaredReference(name.clone())),
            Context::Root { variables, .. } => variables
                .get(name)
                .cloned()
                .ok_or_else(|| ExecutionError::UndeclaredReference(name.clone())),
        }
    }

    pub(crate) fn has_function(&self, name: &Identifier) -> bool {
        match self {
            Context::Root { functions, .. } => functions.has(name),
            Context::Child { parent, .. } => parent.has_function(name),
        }
    }

    pub(crate) fn get_function(&self, name: &Identifier) -> Option<Box<dyn Function>> {
        match self {
            Context::Root { functions, .. } => functions.get(name),
            Context::Child { parent, .. } => parent.get_function(name),
        }
    }

    pub fn add_function<ID, T: 'static, F>(&mut self, name: ID, value: F)
    where
        ID: Into<Identifier>,
        F: Handler<T> + 'static + Send + Sync,
    {
        if let Context::Root { functions, .. } = self {
            functions.add(&name.into(), value);
        };
    }

    pub fn add_operator<ID, T: 'static, F>(&mut self, name: ID, value: F)
    where
        ID: Into<Operator>,
        F: Handler<T> + 'static + Send + Sync,
    {
        if let Context::Root { operators, .. } = self {
            operators.add(&name.into(), value);
        };
    }

    pub(crate) fn get_operator(&self, name: &Operator) -> Option<Box<dyn Function>> {
        match self {
            Context::Root { operators, .. } => operators.get(name),
            Context::Child { parent, .. } => parent.get_operator(name),
        }
    }

    pub fn resolve(&self, expr: &Expression) -> Result<Value, ExecutionError> {
        Value::resolve(expr, self)
    }

    pub fn resolve_all(&self, exprs: &[Expression]) -> Result<Value, ExecutionError> {
        Value::resolve_all(exprs, self)
    }

    pub fn new_inner_scope(&self) -> Context {
        Context::Child {
            parent: self,
            variables: Default::default(),
        }
    }

    /// Constructs a new empty context with no variables or functions.
    ///
    /// If you're looking for a context that has all the standard methods, functions
    /// and macros already added to the context, use [`Context::default`] instead.
    ///
    /// # Example
    /// ```
    /// use cel_interpreter::{Context, Value};
    /// let mut context = Context::empty();
    /// ```
    pub fn empty() -> Self {
        Context::Root {
            variables: Default::default(),
            functions: Default::default(),
            operators: Default::default(),
        }
    }
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        let mut ctx = Context::Root {
            variables: Default::default(),
            functions: Default::default(),
            operators: Default::default(),
        };
        ctx.add_operator(Operator::Arithmetic(ArithmeticOp::Add), add);
        ctx.add_operator(Operator::Arithmetic(ArithmeticOp::Subtract), sub);

        ctx.add_function("contains", functions::contains);
        ctx.add_function("size", functions::size);
        ctx.add_function("has", functions::has);
        ctx.add_function("map", functions::map);
        ctx.add_function("filter", functions::filter);
        ctx.add_function("all", functions::all);
        ctx.add_function("max", functions::max);
        ctx.add_function("startsWith", functions::starts_with);
        ctx.add_function("endsWith", functions::ends_with);
        ctx.add_function("string", functions::string);
        ctx.add_function("to_string", functions::string);
        ctx.add_function("exists", functions::exists);
        ctx.add_function("exists_one", functions::exists_one);

        ctx.add_function("matches", functions::matches);

        {
            ctx.add_function("getFullYear", functions::time::timestamp_year);
            ctx.add_function("getMonth", functions::time::timestamp_month);
            ctx.add_function("getDayOfYear", functions::time::timestamp_year_day);
            ctx.add_function("getDayOfMonth", functions::time::timestamp_month_day);
            ctx.add_function("getDate", functions::time::timestamp_date);
            ctx.add_function("getDayOfWeek", functions::time::timestamp_weekday);
            ctx.add_function("getHours", functions::time::timestamp_hours);
            ctx.add_function("getMinutes", functions::time::timestamp_minutes);
            ctx.add_function("getSeconds", functions::time::timestamp_seconds);
            ctx.add_function("getMilliseconds", functions::time::timestamp_millis);
        }

        ctx
    }
}
