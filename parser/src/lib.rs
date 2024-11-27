use lalrpop_util::lalrpop_mod;

pub mod ast;
pub use ast::*;

pub mod parse;
pub use parse::*;

pub mod error;
pub use error::ParseError;

pub mod duration;
pub use duration::*;

pub mod string;

pub mod numbers;
pub use numbers::*;

pub mod datetime;

pub mod identifiers;

pub mod units;

pub mod operators;

lalrpop_mod!(#[allow(clippy::all)] pub parser, "/cel.rs");

/// Parses a CEL expression and returns it.
///
/// # Example
/// ```
/// use cel_parser::parse;
/// let expr = parse("1 + 1").unwrap();
/// println!("{:?}", expr);
/// ```
pub fn parse(input: &str) -> Result<Expression, ParseError> {
    crate::parser::ExpressionParser::new()
        .parse(input)
        .map_err(|e| ParseError::from_lalrpop(input, e))
}

#[cfg(test)]
mod tests {
    use chrono::TimeZone;
    use rust_decimal_macros::dec;
    use ulid::Ulid;

    use crate::{
        ArithmeticOp, Atom, Atom::*, Expression, Expression::*, Member::*, RelationOp, UnaryOp,
    };

    fn parse(input: &str) -> Expression {
        crate::parse(input).unwrap_or_else(|e| panic!("{}", e))
    }

    fn assert_parse_eq(input: &str, expected: Expression) {
        assert_eq!(parse(input), expected);
    }

    #[test]
    fn ident() {
        assert_parse_eq("a", Ident("a".to_string()));
        assert_parse_eq("hello ", Ident("hello".to_string()));
    }

    #[test]
    fn simple_int() {
        assert_parse_eq("1", Atom(Number(dec!(1))))
    }

    #[test]
    fn simple_float() {
        assert_parse_eq("1.0", Atom(Number(dec!(1.0))))
    }

    #[test]
    fn other_floats() {
        assert_parse_eq("1e3", Expression::Atom(Atom::Number(dec!(1000.0))));
        assert_parse_eq("1e-3", Expression::Atom(Atom::Number(dec!(0.001))));
        assert_parse_eq("1.4e-3", Expression::Atom(Atom::Number(dec!(0.0014))));
    }

    #[test]
    fn single_quote_str() {
        assert_parse_eq("'foobar'", Atom(String("foobar".to_string())))
    }

    #[test]
    fn double_quote_str() {
        assert_parse_eq(r#""foobar""#, Atom(String("foobar".to_string())))
    }

    // #[test]
    // fn single_quote_raw_str() {
    //     assert_parse_eq(
    //         "r'\n'",
    //         Expression::Atom(String("\n".to_string().into())),
    //     );
    // }

    #[test]
    fn single_quote_bytes() {
        assert_parse_eq("b'foo'", Atom(Bytes(b"foo".to_vec())));
        assert_parse_eq("b''", Atom(Bytes(b"".to_vec())));
    }

    #[test]
    fn double_quote_bytes() {
        assert_parse_eq(r#"b"foo""#, Atom(Bytes(b"foo".to_vec())));
        assert_parse_eq(r#"b"""#, Atom(Bytes(b"".to_vec())));
    }

    #[test]
    fn bools() {
        assert_parse_eq("true", Atom(Bool(true)));
        assert_parse_eq("false", Atom(Bool(false)));
    }

    #[test]
    fn nulls() {
        assert_parse_eq("null", Atom(Null));
    }

    #[test]
    fn structure() {
        println!("{:+?}", parse("{1 + a: 3}"));
    }

    #[test]
    fn simple_str() {
        assert_parse_eq(r#"'foobar'"#, Atom(String("foobar".to_string())));
        println!("{:?}", parse(r#"1 == '1'"#))
    }

    #[test]
    fn test_parse_map_macro() {
        assert_parse_eq(
            "[1, 2, 3].map(x, x * 2)",
            FunctionCall(
                Box::new(Ident("map".to_string())),
                Some(Box::new(List(vec![
                    Atom(Number(dec!(1))),
                    Atom(Number(dec!(2))),
                    Atom(Number(dec!(3))),
                ]))),
                vec![
                    Ident("x".to_string()),
                    Arithmetic(
                        Box::new(Ident("x".to_string())),
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
                Member(
                    Ident("a".to_string()).into(),
                    Attribute("b".to_string()).into(),
                )
                .into(),
                Index(Atom(Number(dec!(1))).into()).into(),
            ),
        )
    }

    #[test]
    fn function_call_no_args() {
        assert_parse_eq(
            "a()",
            FunctionCall(Box::new(Ident("a".to_string())), None, vec![]),
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
    fn test_empty_map_parsing() {
        assert_eq!(parse("{}"), (Map(vec![])));
    }

    #[test]
    fn test_nonempty_map_parsing() {
        assert_parse_eq(
            "{'a': 1, 'b': 2}",
            Map(vec![
                (
                    Expression::Atom(String("a".to_string())),
                    Expression::Atom(Number(dec!(1))),
                ),
                (
                    Expression::Atom(String("b".to_string())),
                    Expression::Atom(Number(dec!(2))),
                ),
            ]),
        );
    }

    #[test]
    fn nonempty_map_index_parsing() {
        assert_parse_eq(
            "{'a': 1, 'b': 2}[0]",
            Member(
                Box::new(Map(vec![
                    (
                        Expression::Atom(String("a".to_string())),
                        Expression::Atom(Number(dec!(1))),
                    ),
                    (
                        Expression::Atom(String("b".to_string())),
                        Expression::Atom(Number(dec!(2))),
                    ),
                ])),
                Box::new(Index(Box::new(Expression::Atom(Number(dec!(0)))))),
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

    // #[test]
    // fn binary_product_negated_expressions() {
    //     assert_parse_eq(
    //         "2 * -3",
    //         Arithmetic(
    //             Box::new(Expression::Atom(Atom::Number(dec!(2)))),
    //             ArithmeticOp::Multiply,
    //             Box::new(Unary(
    //                 UnaryOp::Minus,
    //                 Box::new(Expression::Atom(Atom::Number(dec!(3)))),
    //             )),
    //         ),
    //     );
    //
    //     assert_parse_eq(
    //         "2 / -3",
    //         Arithmetic(
    //             Box::new(Expression::Atom(Number(dec!(2)))),
    //             ArithmeticOp::Divide,
    //             Box::new(Unary(
    //                 UnaryOp::Minus,
    //                 Box::new(Expression::Atom(Number(dec!(3)))),
    //             )),
    //         ),
    //     );
    // }

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

        // assert_parse_eq(
        //     "2 - -3",
        //     Arithmetic(
        //         Box::new(Expression::Atom(Atom::Number(dec!(2)))),
        //         ArithmeticOp::Subtract,
        //         Box::new(Unary(
        //             UnaryOp::Minus,
        //             Box::new(Expression::Atom(Atom::Number(dec!(3)))),
        //         )),
        //     ),
        // );
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
                Box::new(Ident("a".to_string())),
                Box::new(Relation(
                    Box::new(Ident("b".to_string())),
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
    fn test_unrecognized_token_error() {
        let source = r#"
            account.balance == transaction.withdrawal
                || (account.overdraftProtection
                    account.overdraftLimit >= transaction.withdrawal  - account.balance)
        "#;

        let err = crate::parse(source).unwrap_err();

        assert_eq!(err.msg, "unrecognized token: 'account'");

        assert_eq!(err.span.start.as_ref().unwrap().line, 3);
        assert_eq!(err.span.start.as_ref().unwrap().column, 20);
        assert_eq!(err.span.end.as_ref().unwrap().line, 3);
        assert_eq!(err.span.end.as_ref().unwrap().column, 27);
    }

    #[test]
    fn test_unrecognized_eof_error() {
        let source = r#" "#;

        let err = crate::parse(source).unwrap_err();

        assert_eq!(err.msg, "unrecognized eof");

        assert_eq!(err.span.start.as_ref().unwrap().line, 0);
        assert_eq!(err.span.start.as_ref().unwrap().column, 0);
        assert_eq!(err.span.end.as_ref().unwrap().line, 0);
        assert_eq!(err.span.end.as_ref().unwrap().column, 0);
    }

    #[test]
    fn test_invalid_token_error() {
        let source = r#"
            account.balance == §
        "#;

        let err = crate::parse(source).unwrap_err();

        assert_eq!(err.msg, "invalid token");

        assert_eq!(err.span.start.as_ref().unwrap().line, 1);
        assert_eq!(err.span.start.as_ref().unwrap().column, 31);
        assert_eq!(err.span.end.as_ref().unwrap().line, 1);
        assert_eq!(err.span.end.as_ref().unwrap().column, 31);
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
