use std::collections::HashSet;

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
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Arithmetic(Box<Expression>, ArithmeticOp, Box<Expression>),
    Relation(Box<Expression>, RelationOp, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Member(Box<Expression>, Box<Member>),
    FunctionCall(Box<Expression>, Option<Box<Expression>>, Vec<Expression>),
    List(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    Atom(Atom),
    Ident(String),
    TagSet(Vec<Expression>),
}

/// Represents a member access in an expression.
#[derive(Debug, PartialEq, Clone)]
pub enum Member {
    Attribute(String),
    Index(Box<Expression>),
    Fields(Vec<(String, Expression)>),
}

/// Represents an atomic value in an expression.
#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Number(rust_decimal::Decimal),
    String(String),
    Bytes(Vec<u8>),
    Bool(bool),
    Null,
    Ulid(ulid::Ulid),
    DateTime(chrono::DateTime<chrono::Utc>),
    Duration(chrono::Duration),
    Tag(String),
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

/// A collection of all the references that an expression makes to variables and functions.
pub struct ExpressionReferences<'expr> {
    variables: HashSet<&'expr str>,
    functions: HashSet<&'expr str>,
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
        self.variables.contains(name.as_ref())
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
        self.functions.contains(name.as_ref())
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
    pub fn variables(&self) -> Vec<&str> {
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
    pub fn functions(&self) -> Vec<&str> {
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
        variables: &mut HashSet<&'expr str>,
        functions: &mut HashSet<&'expr str>,
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
                if let Expression::Ident(v) = &**name {
                    functions.insert(v.as_str());
                }
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
            Expression::Map(v) => {
                for (e1, e2) in v {
                    e1._references(variables, functions);
                    e2._references(variables, functions);
                }
            }
            Expression::Atom(_) => {}
            Expression::Ident(v) => {
                variables.insert(v.as_str());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;

    #[test]
    fn test_references() {
        let expr =
            parse("foo.bar.baz == true && size(foo) > 0 && foo[0] == 1 && bar.startsWith('a')")
                .unwrap();
        let refs = expr.references();
        assert!(refs.has_variable("foo"));
        assert!(refs.has_variable("bar"));
        assert_eq!(refs.variables.len(), 2);

        assert!(refs.has_function("size"));
        assert!(refs.has_function("startsWith"));
        assert_eq!(refs.functions.len(), 2);
    }
}
