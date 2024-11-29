use crate::identifiers::*;
use std::fmt::*;
use std::{collections::HashSet, fmt::Formatter};

/// Turn the raw text into workable tokens.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Atom(Atom),
    Parens(Vec<Token>),
    Brackets(Vec<Token>),
    Braces(Vec<Token>),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Atom(atom) => write!(f, "{}", atom),
            Token::Parens(tokens) => {
                write!(f, "(")?;
                for token in tokens {
                    write!(f, "{}", token)?;
                }
                write!(f, ")")
            }
            Token::Brackets(tokens) => {
                write!(f, "[")?;
                for token in tokens {
                    write!(f, "{}", token)?;
                }
                write!(f, "]")
            }
            Token::Braces(tokens) => {
                write!(f, "{{")?;
                for token in tokens {
                    write!(f, "{}", token)?;
                }
                write!(f, "}}")
            }
        }
    }
}

/// Represents a relational operator in an expression.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum RelationOp {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,
    In,
    MemberOf,
}

/// Represents an arithmetic operator in an expression.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulus,
}

/// Unary operators act on a single operand to the right.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnaryOp {
    Not,
    DoubleNot,
    Minus,
    DoubleMinus,
}

/// Represents an expression in the abstract syntax tree (AST).
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    // Boxed to support recursive expressions.
    Arithmetic(Box<Expression>, ArithmeticOp, Box<Expression>),
    Relation(Box<Expression>, RelationOp, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Member(Box<Expression>, Box<Member>),
    FunctionCall(Identifier, Option<Box<Expression>>, Vec<Expression>),
    List(Vec<Expression>),
    ArgumentList(Vec<Expression>),
    Atom(Atom),
    Ident(Identifier),
    TagSet(Vec<Expression>),
}

/// Represents a member access in an expression.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Member {
    Attribute(Identifier),
    Index(Box<Expression>),
}

/// Represents an atomic value in an expression.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Atom {
    Number(rust_decimal::Decimal),
    String(String),
    Bool(bool),
    Null,
    Ulid(ulid::Ulid),
    DateTime(chrono::DateTime<chrono::Utc>),
    Duration(chrono::Duration),
    HashTag(HashTag),
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Atom::Number(n) => write!(f, "{}", n),
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::Bool(b) => write!(f, "{}", b),
            Atom::Null => write!(f, "null"),
            Atom::Ulid(u) => write!(f, "{}", u),
            Atom::DateTime(d) => write!(f, "{}", d),
            Atom::Duration(d) => write!(f, "{}", d),
            Atom::HashTag(h) => write!(f, "{}", h),
        }
    }
}

impl From<ulid::Ulid> for Atom {
    fn from(ulid: ulid::Ulid) -> Self {
        Atom::Ulid(ulid)
    }
}

impl From<rust_decimal::Decimal> for Atom {
    fn from(decimal: rust_decimal::Decimal) -> Self {
        Atom::Number(decimal)
    }
}

impl From<chrono::DateTime<chrono::Utc>> for Atom {
    fn from(datetime: chrono::DateTime<chrono::Utc>) -> Self {
        Atom::DateTime(datetime)
    }
}

impl From<chrono::Duration> for Atom {
    fn from(duration: chrono::Duration) -> Self {
        Atom::Duration(duration)
    }
}

impl From<bool> for Atom {
    fn from(b: bool) -> Self {
        Atom::Bool(b)
    }
}

impl From<String> for Atom {
    fn from(s: String) -> Self {
        Atom::String(s)
    }
}

impl From<&str> for Atom {
    fn from(s: &str) -> Self {
        Atom::String(s.to_string())
    }
}

impl From<HashTag> for Atom {
    fn from(tag: HashTag) -> Self {
        Atom::HashTag(tag)
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

    /// Returns true if the expression references the provided function name.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("size(foo) > 0").unwrap();
    /// let references = expression.references();
    /// assert!(references.has_function("size"));
    /// ```
    pub fn has_function(&self, name: impl AsRef<str>) -> bool {
        let identifier: Identifier = name.as_ref().into();
        self.functions.contains(&identifier)
    }

    /// Returns a list of all variables referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("foo.bar == true").unwrap();
    /// let references = expression.references();
    /// assert_eq!(vec!["foo"], references.variables());
    /// ```
    pub fn variables(&self) -> Vec<&Identifier> {
        self.variables.iter().copied().collect()
    }

    /// Returns a list of all functions referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("size(foo) > 0").unwrap();
    /// let references = expression.references();
    /// assert_eq!(vec!["size"], references.functions());
    /// ```
    pub fn functions(&self) -> Vec<&Identifier> {
        self.functions.iter().copied().collect()
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
    /// assert!(references.has_function("size"));
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
            Expression::Arithmetic(e1, _, e2)
            | Expression::Relation(e1, _, e2)
            | Expression::Ternary(e1, _, e2)
            | Expression::Or(e1, e2)
            | Expression::And(e1, e2) => {
                e1._references(variables, functions);
                e2._references(variables, functions);
            }
            Expression::Unary(_, e) => {
                e._references(variables, functions);
            }
            Expression::Member(e, _) => {
                e._references(variables, functions);
            }
            Expression::FunctionCall(name, target, args) => {
                functions.insert(name);
                if let Some(target) = target {
                    target._references(variables, functions);
                }
                for e in args {
                    e._references(variables, functions);
                }
            }
            Expression::List(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
            Expression::TagSet(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
            Expression::ArgumentList(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
            Expression::Atom(_) => {}
            Expression::Ident(v) => {
                variables.insert(v);
            }
        }
    }
}
