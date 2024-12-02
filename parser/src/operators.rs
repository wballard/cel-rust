/// Keep the operators here.
///
use chumsky::prelude::*;

/// Represents a relational operator in an expression.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum RelationOp {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,
    In,
    GetMember,
}

/// Represents an arithmetic operator in an expression.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulus,
    Exponent,
}

/// Represents a boolean operator in an expression.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum LogicalOp {
    And,
    Or,
    Not,
}

/// These are enumerated into categories to provide a bit more sepantics for
/// the parser and to make it easier to understand the AST.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Operator {
    Relation(RelationOp),
    Arithmetic(ArithmeticOp),
    Logical(LogicalOp),
}

pub fn parse_relation_op<'a>() -> impl Parser<'a, &'a str, RelationOp, extra::Err<Rich<'a, char>>> {
    let op = |c| just(c).padded();
    // watch the order of precedence here -- long tokens with the same prefix
    // need to go ahead of short tokens
    choice((
        op("<=").map(|_| RelationOp::LessThanEq),
        op(">=").map(|_| RelationOp::GreaterThanEq),
        op("==").map(|_| RelationOp::Equals),
        op("!=").map(|_| RelationOp::NotEquals),
        op("in").map(|_| RelationOp::In),
        op("<").map(|_| RelationOp::LessThan),
        op(">").map(|_| RelationOp::GreaterThan),
        op(".").map(|_| RelationOp::GetMember),
    ))
}

pub fn parse_logical_op<'a>() -> impl Parser<'a, &'a str, LogicalOp, extra::Err<Rich<'a, char>>> {
    let op = |c| just(c).padded();
    choice((
        op("&&").map(|_| LogicalOp::And),
        op("||").map(|_| LogicalOp::Or),
        op("!").map(|_| LogicalOp::Not),
    ))
}

pub fn parse_arithmetic_op<'a>(
) -> impl Parser<'a, &'a str, ArithmeticOp, extra::Err<Rich<'a, char>>> {
    let op = |c| just(c).padded();
    choice((
        op("+").map(|_| ArithmeticOp::Add),
        op("-").map(|_| ArithmeticOp::Subtract),
        op("*").map(|_| ArithmeticOp::Multiply),
        op("/").map(|_| ArithmeticOp::Divide),
        op("%").map(|_| ArithmeticOp::Modulus),
        op("^").map(|_| ArithmeticOp::Exponent),
    ))
}

/// Parse all supported operators.
pub fn parse_op<'a>() -> impl Parser<'a, &'a str, Operator, extra::Err<Rich<'a, char>>> {
    choice((
        parse_relation_op().boxed().map(Operator::Relation),
        parse_logical_op().boxed().map(Operator::Logical),
        parse_arithmetic_op().boxed().map(Operator::Arithmetic),
    ))
}
