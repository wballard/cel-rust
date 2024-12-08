use crate::identifiers::*;
use crate::lexer::*;
use std::collections::HashSet;
use std::fmt::*;

/// Represents an expression in the abstract syntax tree (AST).
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    // Boxed to support recursive expressions.
    Unary(Operator, Box<Expression>),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(Box<Expression>, Box<Expression>),
    Indexer(Box<Expression>, Box<Expression>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Set(Vec<Expression>),
    Atom(Atom),
    Identifier(Identifier),
    Multiple(Vec<Expression>),
    Empty,
}

impl Expression {
    /// Used in function calls to get at the arguments.
    pub fn to_arguments(&self) -> Vec<&Expression> {
        match self {
            Expression::Tuple(e) => e.iter().collect(),
            Expression::Atom(_) => vec![self],
            _ => vec![],
        }
    }
}

/// A collection of all the references that an expression makes to variables and functions.
pub struct ExpressionReferences<'expr> {
    variables: HashSet<&'expr Identifier>,
    functions: HashSet<&'expr Identifier>,
}

impl<'expr> ExpressionReferences<'expr> {
    /// Returns true if the expression references the provided variable name.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("foo.bar == true").unwrap();
    /// let references = expression.references();
    /// assert!(references.has_variable("foo"));
    /// ```
    pub fn has_variable(&self, name: impl AsRef<str>) -> bool {
        let identifier: Identifier = name.as_ref().into();
        self.variables.contains(&identifier)
    }

    /// Returns a list of all variables referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("foo.bar == true").unwrap();
    /// let references = expression.references();
    /// assert!(references.has_variable("foo"));
    /// ```
    pub fn variables(&self) -> Vec<&Identifier> {
        self.variables.iter().copied().collect()
    }
}

impl Expression {
    /// Returns a set of all variables and functions referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("foo && size(foo) > 0").unwrap();
    /// let references = expression.references();
    ///
    /// assert!(references.has_variable("foo"));
    /// ```
    pub fn references(&self) -> ExpressionReferences {
        let mut variables = HashSet::new();
        let mut functions = HashSet::new();
        self._references(&mut variables, &mut functions);
        ExpressionReferences {
            variables,
            functions,
        }
    }

    /// Internal recursive function to collect all variable and function references in the expression.
    fn _references<'expr>(
        &'expr self,
        variables: &mut HashSet<&'expr Identifier>,
        functions: &mut HashSet<&'expr Identifier>,
    ) {
        match self {
            Expression::Unary(_, e) => {
                e._references(variables, functions);
            }
            Expression::Binary(e1, _, e2) => {
                e1._references(variables, functions);
                e2._references(variables, functions);
            }
            Expression::Ternary(e1, e2, e3) => {
                e1._references(variables, functions);
                e2._references(variables, functions);
                e3._references(variables, functions);
            }
            Expression::FunctionCall(left, right) => {
                left._references(variables, functions);
                right._references(variables, functions);
            }
            Expression::Indexer(left, right) => {
                left._references(variables, functions);
                right._references(variables, functions);
            }
            Expression::List(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
            Expression::Set(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
            Expression::Tuple(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
            Expression::Atom(_) => {}
            Expression::Identifier(v) => {
                variables.insert(v);
            }
            Expression::Empty => {}
            Expression::Multiple(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
        }
    }
}
