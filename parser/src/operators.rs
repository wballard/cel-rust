use crate::ast::*;
use chumsky::prelude::*;

pub fn parse_relation_op<'a>() -> impl Parser<'a, &'a str, RelationOp, extra::Err<Rich<'a, char>>> {
    let op = |c| just(c).padded();
    choice((
        op("<").map(|_| RelationOp::LessThan),
        op("<=").map(|_| RelationOp::LessThanEq),
        op(">").map(|_| RelationOp::GreaterThan),
        op(">=").map(|_| RelationOp::GreaterThanEq),
        op("==").map(|_| RelationOp::Equals),
        op("!=").map(|_| RelationOp::NotEquals),
        op("in").map(|_| RelationOp::In),
    ))
}

pub fn parse_unary_op<'a>() -> impl Parser<'a, &'a str, UnaryOp, extra::Err<Rich<'a, char>>> {
    let op = |c| just(c).padded();
    choice((
        op("!").map(|_| UnaryOp::Not),
        op("!!").map(|_| UnaryOp::DoubleNot),
        op("-").map(|_| UnaryOp::Minus),
        op("--").map(|_| UnaryOp::DoubleMinus),
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
    ))
}
