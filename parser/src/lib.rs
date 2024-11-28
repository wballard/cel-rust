use chumsky::prelude::*;

pub mod error;
pub use error::*;

pub mod ast;
pub use ast::*;

pub mod string;
pub use string::*;

pub mod numbers;
pub use numbers::*;

pub mod datetime;
pub use datetime::*;

pub mod identifiers;
pub use identifiers::*;

pub mod units;
pub use units::*;

pub mod operators;
pub use operators::*;

/// Parses a CEL expression into an AST ready for analysis and evaluation.
///
/// # Example
/// ```
/// use cel_parser::parse;
/// let expr = parse("1 + 1").unwrap();
/// println!("{:?}", expr);
/// ```
pub fn parse(input: &str) -> Result<Expression, ParseErrors> {
    let (expression, errors) = parse_expression().parse(input).into_output_errors();
    if errors.is_empty() {
        Ok(expression.unwrap())
    } else {
        Err(ParseErrors::from_chumsky(errors))
    }
}

fn parse_atom<'a>() -> impl Parser<'a, &'a str, Atom, extra::Err<Rich<'a, char>>> {
    // atoms form the base of the expression tree
    choice((
        parse_string().map(Atom::String),
        parse_number().map(Atom::Number),
        parse_bool().map(Atom::Bool),
        parse_ulid().map(Atom::Ulid),
        parse_datetime().map(Atom::DateTime),
        parse_duration().map(Atom::Duration),
        parse_hashtag().map(Atom::HashTag),
    ))
}

fn parse_expression<'a>() -> impl Parser<'a, &'a str, Expression, extra::Err<Rich<'a, char>>> {
    recursive(|expression| {
        let atom = parse_atom().map(Expression::Atom);
        let identifier = parse_identifier().map(Expression::Ident);
        let list = expression
            .delimited_by(just('['), just(']'))
            .separated_by(just(','))
            .allow_trailing()
            .collect()
            .map(Expression::List);
        list
    })
}

#[cfg(test)]
mod tests {
    use crate::Expression;
    use chrono::TimeZone;
    use rstest::rstest;
    use rust_decimal_macros::dec;
    use ulid::Ulid;

    use crate::{ArithmeticOp, Atom, Atom::*, Expression::*, Member::*, RelationOp, UnaryOp};

    fn parse(input: &str) -> Expression {
        crate::parse(input).unwrap_or_else(|e| panic!("{}", e))
    }

    fn assert_parse_eq(input: &str, expected: Expression) {
        assert_eq!(parse(input), expected);
    }
    #[rstest]
    #[case("a", Ident("a".into()))]
    #[case("hello ", Ident("hello".into()))]
    fn identifiers(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("1", Atom(Number(dec!(1))))]
    #[case("1.0", Atom(Number(dec!(1.0))))]
    #[case("1e3", Expression::Atom(Atom::Number(dec!(1000.0))))]
    #[case("1e-3", Expression::Atom(Atom::Number(dec!(0.001))))]
    #[case("1.4e-3", Expression::Atom(Atom::Number(dec!(0.0014))))]
    fn numbers(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("'foobar'", Atom(String("foobar".to_string())))]
    #[case("r'\n'", Atom(String("\n".to_string())))]
    #[case(r#""foobar""#, Atom(String("foobar".to_string())))]
    fn strings(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("true", Atom(Bool(true)))]
    #[case("false", Atom(Bool(false)))]
    fn bools(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[test]
    fn test_parse_map_macro() {
        assert_parse_eq(
            "[1, 2, 3].map(x, x * 2)",
            FunctionCall(
                Box::new(Ident("map".into())),
                Some(Box::new(List(vec![
                    Atom(Number(dec!(1))),
                    Atom(Number(dec!(2))),
                    Atom(Number(dec!(3))),
                ]))),
                vec![
                    Ident("x".into()),
                    Arithmetic(
                        Box::new(Ident("x".into())),
                        ArithmeticOp::Multiply,
                        Box::new(Atom(Number(dec!(2)))),
                    ),
                ],
            ),
        )
    }

    #[test]
    fn nested_attributes() {
        assert_parse_eq(
            "a.b[1]",
            Member(
                Member(Ident("a".into()).into(), Attribute("b".into()).into()).into(),
                Index(Atom(Number(dec!(1))).into()).into(),
            ),
        )
    }

    #[test]
    fn function_call_no_args() {
        assert_parse_eq(
            "a()",
            FunctionCall(Box::new(Ident("a".into())), None, vec![]),
        );
    }

    #[test]
    fn test_parser_bool_unary_ops() {
        assert_parse_eq(
            "!false",
            Unary(UnaryOp::Not, Box::new(Expression::Atom(Atom::Bool(false)))),
        );
        assert_parse_eq(
            "!true",
            Unary(UnaryOp::Not, Box::new(Expression::Atom(Atom::Bool(true)))),
        );
    }

    #[test]
    fn test_parser_binary_bool_expressions() {
        assert_parse_eq(
            "true == true",
            Relation(
                Box::new(Expression::Atom(Atom::Bool(true))),
                RelationOp::Equals,
                Box::new(Expression::Atom(Atom::Bool(true))),
            ),
        );
    }

    #[test]
    fn test_parser_bool_unary_ops_repeated() {
        assert_eq!(
            parse("!!true"),
            (Unary(
                UnaryOp::DoubleNot,
                Box::new(Expression::Atom(Atom::Bool(true))),
            ))
        );
    }

    #[test]
    fn delimited_expressions() {
        assert_parse_eq(
            "(-((1)))",
            Unary(
                UnaryOp::Minus,
                Box::new(Expression::Atom(Atom::Number(dec!(1)))),
            ),
        );
    }

    #[test]
    fn test_empty_list_parsing() {
        assert_eq!(parse("[]"), (List(vec![])));
    }

    #[test]
    fn test_int_list_parsing() {
        assert_parse_eq(
            "[1,2,3]",
            List(vec![
                Expression::Atom(Atom::Number(dec!(1))),
                Expression::Atom(Atom::Number(dec!(2))),
                Expression::Atom(Atom::Number(dec!(3))),
            ]),
        );
    }

    #[test]
    fn list_index_parsing() {
        assert_parse_eq(
            "[1,2,3][0]",
            Member(
                Box::new(List(vec![
                    Expression::Atom(Number(dec!(1))),
                    Expression::Atom(Number(dec!(2))),
                    Expression::Atom(Number(dec!(3))),
                ])),
                Box::new(Index(Box::new(Expression::Atom(Number(dec!(0)))))),
            ),
        );
    }

    #[test]
    fn mixed_type_list() {
        assert_parse_eq(
            "['0', 1, 3.0, null]",
            List(vec![
                Expression::Atom(String("0".to_string())),
                Expression::Atom(Number(dec!(1))),
                Expression::Atom(Number(dec!(3.0))),
                Expression::Atom(Null),
            ]),
        );
    }

    #[test]
    fn test_nested_list_parsing() {
        assert_parse_eq(
            "[[], [], [[1]]]",
            List(vec![
                List(vec![]),
                List(vec![]),
                List(vec![List(vec![Expression::Atom(Number(dec!(1)))])]),
            ]),
        );
    }

    #[test]
    fn test_in_list_relation() {
        assert_parse_eq(
            "2 in [2]",
            Relation(
                Box::new(Expression::Atom(Number(dec!(2)))),
                RelationOp::In,
                Box::new(List(vec![Expression::Atom(Number(dec!(2)))])),
            ),
        );
    }

    #[test]
    fn integer_relations() {
        assert_parse_eq(
            "2 != 3",
            Relation(
                Box::new(Expression::Atom(Number(dec!(2)))),
                RelationOp::NotEquals,
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );
        assert_parse_eq(
            "2 == 3",
            Relation(
                Box::new(Expression::Atom(Number(dec!(2)))),
                RelationOp::Equals,
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );

        assert_parse_eq(
            "2 < 3",
            Relation(
                Box::new(Expression::Atom(Number(dec!(2)))),
                RelationOp::LessThan,
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );

        assert_parse_eq(
            "2 <= 3",
            Relation(
                Box::new(Expression::Atom(Number(dec!(2)))),
                RelationOp::LessThanEq,
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );
    }

    #[test]
    fn binary_product_expressions() {
        assert_parse_eq(
            "2 * 3",
            Arithmetic(
                Box::new(Expression::Atom(Atom::Number(dec!(2)))),
                ArithmeticOp::Multiply,
                Box::new(Expression::Atom(Atom::Number(dec!(3)))),
            ),
        );
    }

    #[test]
    fn test_parser_sum_expressions() {
        assert_parse_eq(
            "2 + 3",
            Arithmetic(
                Box::new(Expression::Atom(Atom::Number(dec!(2)))),
                ArithmeticOp::Add,
                Box::new(Expression::Atom(Atom::Number(dec!(3)))),
            ),
        );

        assert_parse_eq(
            "2 - -3",
            Arithmetic(
                Box::new(Expression::Atom(Atom::Number(dec!(2)))),
                ArithmeticOp::Subtract,
                Box::new(Expression::Atom(Atom::Number(dec!(-3)))),
            ),
        );
    }

    #[test]
    fn conditionals() {
        assert_parse_eq(
            "true && true",
            And(
                Box::new(Expression::Atom(Bool(true))),
                Box::new(Expression::Atom(Bool(true))),
            ),
        );
        assert_parse_eq(
            "false || true",
            Or(
                Box::new(Expression::Atom(Bool(false))),
                Box::new(Expression::Atom(Bool(true))),
            ),
        );
    }
    #[test]
    fn test_ternary_true_condition() {
        assert_parse_eq(
            "true ? 'result_true' : 'result_false'",
            Ternary(
                Box::new(Expression::Atom(Bool(true))),
                Box::new(Expression::Atom(String("result_true".to_string()))),
                Box::new(Expression::Atom(String("result_false".to_string()))),
            ),
        );

        assert_parse_eq(
            "true ? 100 : 200",
            Ternary(
                Box::new(Expression::Atom(Bool(true))),
                Box::new(Expression::Atom(Number(dec!(100)))),
                Box::new(Expression::Atom(Number(dec!(200)))),
            ),
        );
    }

    #[test]
    fn test_ternary_false_condition() {
        assert_parse_eq(
            "false ? 'result_true' : 'result_false'",
            Ternary(
                Box::new(Expression::Atom(Bool(false))),
                Box::new(Expression::Atom(String("result_true".to_string()))),
                Box::new(Expression::Atom(String("result_false".to_string()))),
            ),
        );
    }

    #[test]
    fn test_operator_precedence() {
        assert_parse_eq(
            "a && b == 'string'",
            And(
                Box::new(Ident("a".into())),
                Box::new(Relation(
                    Box::new(Ident("b".into())),
                    RelationOp::Equals,
                    Box::new(Expression::Atom(String("string".to_string()))),
                )),
            ),
        );
    }

    #[test]
    fn test_foobar() {
        println!("{:?}", parse("foo.bar.baz == 10 && size(requests) == 3"))
    }

    #[test]
    fn test_ulid() {
        assert_parse_eq(
            "01D39ZY06FGSCTVN4T2V9PKHFZ",
            Atom(
                Ulid::from_string("01D39ZY06FGSCTVN4T2V9PKHFZ")
                    .unwrap()
                    .into(),
            ),
        )
    }

    #[test]
    fn test_date() {
        assert_parse_eq(
            "2021-01-01",
            Atom(
                chrono::Utc
                    .with_ymd_and_hms(2021, 1, 1, 0, 0, 0)
                    .unwrap()
                    .into(),
            ),
        )
    }

    #[test]
    fn test_date_time() {
        assert_parse_eq(
            "2021-01-01T14:20:01",
            Atom(
                chrono::Utc
                    .with_ymd_and_hms(2021, 1, 1, 14, 20, 1)
                    .unwrap()
                    .into(),
            ),
        )
    }

    #[test]
    fn test_date_time_zone() {
        assert_parse_eq(
            "2021-01-01T14:20:01-08:00",
            Atom(
                chrono::Utc
                    .with_ymd_and_hms(2021, 1, 1, 22, 20, 1)
                    .unwrap()
                    .into(),
            ),
        )
    }

    #[test]
    fn test_duration() {
        assert_parse_eq("1h", Atom(chrono::Duration::hours(1).into()))
    }

    #[test]
    fn test_duration_compound() {
        assert_parse_eq(
            "1h1m1s",
            Atom(
                (chrono::Duration::hours(1)
                    + chrono::Duration::minutes(1)
                    + chrono::Duration::seconds(1))
                .into(),
            ),
        )
    }

    #[test]
    fn test_tag() {
        assert_parse_eq("#foobar", Atom(HashTag("foobar".into())))
    }

    #[test]
    fn test_tag_space() {
        assert_parse_eq("#foo_bar", Atom(HashTag("foo_bar".into())))
    }

    #[test]
    fn test_tag_set() {
        assert_parse_eq(
            "{# #foo_bar, #💕 #}",
            TagSet(vec![
                Atom(HashTag("foo_bar".into())),
                Atom(HashTag("💕".into())),
            ]),
        )
    }
}
