extern crate core;

use cel_parser::*;
use std::convert::TryFrom;
use thiserror::Error;

mod macros;

pub mod context;
pub use cel_parser::error::ParseErrors;
pub use cel_parser::Expression;
pub use context::Context;
pub use functions::FunctionContext;
pub use objects::{ResolveResult, Value, ValueList, ValueSet};
mod function_registry;
pub mod functions;
mod magic;
pub mod objects;
mod resolvers;
use magic::FromContext;

// ops!
mod operators;

#[derive(Error, Debug, PartialEq, Clone)]
pub enum ExecutionError {
    #[error("Invalid argument count: expected {expected}, got {actual}")]
    InvalidArgumentCount { expected: usize, actual: usize },
    #[error("Invalid argument type: {:?}", .target)]
    UnsupportedTargetType { target: Value },
    #[error("Method '{method}' not supported on type '{target:?}'")]
    NotSupportedAsMethod { method: String, target: Value },
    /// Indicates that the script attempted to use a value as a key in a map,
    /// but the type of the value was not supported as a key.
    #[error("Unable to use value '{0:?}' as a key")]
    UnsupportedKeyType(Value),
    #[error("Unexpected type: got '{got}', want '{want}'")]
    UnexpectedType { got: String, want: String },
    /// Indicates that the script attempted to reference a key on a type that
    /// was missing the requested key.
    #[error("No such key: {0}")]
    NoSuchKey(String),
    /// Indicates that the script attempted to reference an undeclared variable
    /// method, or function.
    #[error("Undeclared reference to '{0}'")]
    UndeclaredReference(Identifier),
    /// Indicates that a function expected to be called as a method, or to be
    /// called with at least one parameter.
    #[error("Missing argument or target")]
    MissingArgumentOrTarget,
    /// Indicates that a comparison could not be performed.
    #[error("{0:?} can not be compared to {1:?}")]
    ValuesNotComparable(Value, Value),
    /// Indicates that an operator was used on a type that does not support it.
    #[error("Unsupported unary operator '{0}': {1:?}")]
    UnsupportedUnaryOperator(&'static str, Value),
    /// Attemping to use an operator that is not defined.
    #[error("Undefined operator '{0}'")]
    UndefinedOperator(Operator),
    /// Indicates that an unsupported binary operator was applied on two values
    /// where it's unsupported, for example list + map.
    #[error("Unsupported binary operator '{0}': {1:?}, {2:?}")]
    UnsupportedBinaryOperator(Operator, Value, Value),
    /// Indicates that an unsupported type was used to index a map
    #[error("Cannot use value as map index: {0:?}")]
    UnsupportedMapIndex(Value),
    /// Indicates that an unsupported type was used to index a list
    #[error("Cannot use value as list index: {0:?}")]
    UnsupportedListIndex(Value),
    /// Indicates that an unsupported type was used to index a list
    #[error("Cannot use value {0:?} to index {1:?}")]
    UnsupportedIndex(Value, Value),
    /// Indicates that a [`Member::Fields`] construction was attempted
    /// which is not yet supported.
    #[error("Unsupported fields construction: {0:?}")]
    UnsupportedFieldsConstruction(Member),
    /// Indicates that a function had an error during execution.
    #[error("Error executing function '{function}': {message}")]
    FunctionError {
        function: Identifier,
        message: String,
    },
}

impl ExecutionError {
    pub fn undeclared_reference(name: Identifier) -> Self {
        ExecutionError::UndeclaredReference(name)
    }

    pub fn invalid_argument_count(expected: usize, actual: usize) -> Self {
        ExecutionError::InvalidArgumentCount { expected, actual }
    }

    pub fn function_error<E: ToString>(function: Identifier, error: E) -> Self {
        ExecutionError::FunctionError {
            function,
            message: error.to_string(),
        }
    }

    pub fn unsupported_target_type(target: Value) -> Self {
        ExecutionError::UnsupportedTargetType { target }
    }

    pub fn not_supported_as_method(method: &str, target: Value) -> Self {
        ExecutionError::NotSupportedAsMethod {
            method: method.to_string(),
            target,
        }
    }

    pub fn unsupported_key_type(value: Value) -> Self {
        ExecutionError::UnsupportedKeyType(value)
    }

    pub fn missing_argument_or_target() -> Self {
        ExecutionError::MissingArgumentOrTarget
    }
}

#[derive(Debug)]
pub struct Program {
    expression: Expression,
}

impl Program {
    pub fn compile(source: &str) -> Result<Program, ParseErrors> {
        parse(source).map(|expression| Program { expression })
    }

    pub fn execute(&self, context: &Context) -> ResolveResult {
        Value::resolve(&self.expression, context)
    }

    /// Returns the variables and functions referenced by the CEL program
    ///
    /// # Example
    /// ```rust
    /// # use cel_interpreter::Program;
    /// let program = Program::compile("size(foo) > 0").unwrap();
    /// let references = program.references();
    ///
    /// assert!(references.has_function("size"));
    /// assert!(references.has_variable("foo"));
    /// ```
    pub fn references(&self) -> ExpressionReferences {
        self.expression.references()
    }
}

impl TryFrom<&str> for Program {
    type Error = ParseErrors;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Program::compile(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::objects::ResolveResult;
    use crate::Program;
    use rstest::rstest;
    use std::convert::TryInto;
    use std::fs;
    use std::path::PathBuf;

    /// Tests the provided script and returns the result. An optional context can be provided.
    pub(crate) fn test_script(script: &str, ctx: Option<Context>) -> ResolveResult {
        let program = Program::compile(script).unwrap();
        program.execute(&ctx.unwrap_or_default())
    }

    fn check_script(script: &str, expected: &str) {
        let program = Program::compile(script).unwrap();
        print!("{:?} => ", program);
        let context = Context::default();
        let result = program.execute(&context);
        print!("{:?} => ", result);
        assert_eq!(result.unwrap().to_string(), expected.to_string());
    }

    #[rstest]
    fn simple_expressions(#[files("tests/scripts/simple/**/*.cel")] path: PathBuf) {
        let content = fs::read_to_string(path).expect("Unable to read file");
        // then there must be a script
        assert!(!content.is_empty());
        // separate the input and the expected output
        let parts: Vec<&str> = content.split("---").collect();
        // with a script and an answer
        let script = parts[0];
        let answer = parts[1];
        check_script(script, answer.trim());
    }

    #[test]
    fn parse() {
        Program::compile("1 + 1").unwrap();
    }

    #[test]
    fn from_str() {
        let input = "1.1";
        let _p: Program = input.try_into().unwrap();
    }
}
