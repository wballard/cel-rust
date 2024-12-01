use chumsky::input::BorrowInput;
use chumsky::input::Input;
use chumsky::input::ValueInput;
use chumsky::pratt::*;
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

mod lexer;
use lexer::*;

mod atoms;
pub use atoms::*;

/// Parses a CEL expression into an AST ready for analysis and evaluation.
///
/// # Example
/// ```
/// use cel_parser::parse;
/// let expr = parse("1 + 1").unwrap();
/// println!("{:?}", expr);
/// ```
pub fn parse(input: &str) -> Result<Expression, ParseErrors> {
    // get the raw text turned into tokens -- then we can work with some structure
    let (tokens, errors) = lexer().parse(input).into_output_errors();
    if errors.is_empty() {
        match tokens {
            Some(tokens) => {
                let (expr, errors) = parse_expression(make_input)
                    .parse(make_input((0..tokens.len()).into(), &tokens))
                    .into_output_errors();
                if errors.is_empty() {
                    Ok(expr.unwrap())
                } else {
                    Err(ParseErrors::from_parser(errors))
                }
            }
            None => {
                // no tokens, so this is false-y
                Ok(Expression::Atom(Atom::Bool(false)))
            }
        }
    } else {
        Err(ParseErrors::from_text(errors))
    }
}

pub fn parse_expression<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Expression, extra::Err<Rich<'src, Token>>>
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
    M: Fn(SimpleSpan, &'src [Spanned<Token>]) -> I + Clone + 'src,
{
    let atom = select_ref! { Token::Atom(x) => Expression::Atom(x.clone()) };
    let identifier = select_ref! { Token::Identifier(x) => Expression::Identifier(x.clone()) };
    let atoms = choice((identifier, atom));
    recursive(|expression| {
        choice((
            // empty expression at end of sequence, this us used to delimit lists
            end().map(|_| Expression::Empty),
            // simple atoms
            atoms,
            // compound nests
            expression
                .nested_in(select_ref! { Token::Brackets(ts) = e => {
                    // eat trailing separators
                    match ts.last() {
                        Some((Token::Separator, _)) => {
                                    let trim = ts.split_last().unwrap().1;
                                    make_input(e.span(), trim)
                                }
                        _ => make_input(e.span(), ts)
                    }
                }})
                .map(|nested| match nested {
                    // picking out the nested expressions and turning them into a list
                    Expression::Multiple(exprs) => Expression::List(exprs),
                    Expression::Empty => Expression::List(vec![]),
                    // this is a sensible default -- any single item just becomes a list
                    any => Expression::List(vec![any]),
                }),
        ))
        .pratt((
            // join the atoms together
            infix(left(10), just(Token::Separator), |left, right| {
                // flatten out as we go from the left associations of lists members
                let ll = match left {
                    Expression::Multiple(items) => items,
                    any => vec![any],
                };
                let rr = match right {
                    Expression::Multiple(items) => items,
                    any => vec![any],
                };
                Expression::Multiple([ll, rr].concat())
            }),
            // logical operators
            prefix(
                1000,
                just(Token::Operator(Operator::Logical(LogicalOp::Not))),
                |right| Expression::Unary(Operator::Logical(LogicalOp::Not), Box::new(right)),
            ),
            infix(
                left(100),
                just(Token::Operator(Operator::Logical(LogicalOp::And))),
                |left, right| {
                    Expression::Binary(
                        Box::new(left),
                        Operator::Logical(LogicalOp::And),
                        Box::new(right),
                    )
                },
            ),
        ))
    })
    .map(|x| x)

    /*
    let identifier = parse_identifier().map(Expression::Ident).boxed();

    // and this is the expression recursive parser
    recursive(|expression| {
        let expression_list = expression
            .clone()
            .separated_by(just(','))
            .allow_trailing()
            .collect();
        let list = expression_list
            .clone()
            .delimited_by(just('['), just(']'))
            .map(Expression::List);
        let argument_list = expression_list
            .clone()
            .delimited_by(just('('), just(')'))
            .map(Expression::ArgumentList);
        let tagset = expression_list
            .clone()
            .delimited_by(just('{'), just('}'))
            .map(Expression::TagSet);

        //choice((function_call, atom, identifier, list, tagset)).padded()
        // starting from the 'left' -- an expression will have an initial atom or identifier
        //let operand = choice((atom, identifier, list, argument_list, tagset)).padded();
        let operand = choice((atom, list)).padded();

        let p = operand
            .pratt(vec![
                //unary operators
                prefix(10, just('!'), |_, e| {
                    Ok(Expression::Unary(UnaryOp::Not, Box::new(e)))
                }),
            ])
            .clone();

        operand
    })
    */
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expression::*;
    use crate::atoms::Atom::*;
    use crate::operators::ArithmeticOp;
    use crate::operators::LogicalOp;
    use crate::operators::RelationOp;
    use chrono::TimeZone;
    use rstest::rstest;
    use rust_decimal_macros::dec;

    fn parse(input: &str) -> Expression {
        crate::parse(input).unwrap_or_else(|e| panic!("{}", e))
    }

    fn assert_parse_eq(input: &str, expected: Expression) {
        assert_eq!(parse(input), expected);
    }
    #[rstest]
    #[case("a", Identifier("a".into()))]
    #[case("hello ", Identifier("hello".into()))]
    fn identifiers(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("1", Atom(Number(dec!(1))))]
    #[case("1.0", Atom(Number(dec!(1.0))))]
    #[case("1e3", Expression::Atom(Number(dec!(1000.0))))]
    #[case("1e-3", Expression::Atom(Number(dec!(0.001))))]
    #[case("1.4e-3", Expression::Atom(Number(dec!(0.0014))))]
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

    #[rstest]
    #[case("#one", Atom(HashTag("one".into())))]
    #[case("#🦃", Atom(HashTag("🦃".into())))]
    fn hashtags(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("&01ARZ3NDEKTSV4RRFFQ69G5FAV", Atom(::ulid::Ulid::from_string("01ARZ3NDEKTSV4RRFFQ69G5FAV").unwrap().into()))]
    fn ulid(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("[]", List(vec![
    ]))]
    #[case("[1]", List(vec![
        Atom(Number(dec!(1))),
    ]))]
    #[case("[1, 2, 3]", List(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    #[case("[1, 2, 3,]", List(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    fn list(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("()", List(vec![
    ]))]
    #[case("(1)", List(vec![
        Atom(Number(dec!(1))),
    ]))]
    #[case("(1, 2, 3)", List(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    #[case("(1, 2, 3,)", List(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    fn argument_list(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[test]
    fn to_string() {
        assert_parse_eq(
            "[1, 2, 3].string()",
            FunctionCall(
                "string".into(),
                Some(Box::new(List(vec![
                    Atom(Number(dec!(1))),
                    Atom(Number(dec!(2))),
                    Atom(Number(dec!(3))),
                ]))),
                vec![],
            ),
        )
    }

    #[test]
    fn map_list_constant() {
        assert_parse_eq(
            "[1, 2, 3].map(x, x * 2)",
            FunctionCall(
                "map".into(),
                Some(Box::new(List(vec![
                    Atom(Number(dec!(1))),
                    Atom(Number(dec!(2))),
                    Atom(Number(dec!(3))),
                ]))),
                vec![
                    Identifier("x".into()),
                    Binary(
                        Box::new(Identifier("x".into())),
                        Operator::Arithmetic(ArithmeticOp::Multiply),
                        Box::new(Atom(Number(dec!(2)))),
                    ),
                ],
            ),
        )
    }

    /*
    TODO: this is a slice
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
    */

    #[test]
    fn function_call_no_args() {
        assert_parse_eq("a()", FunctionCall("a".into(), None, vec![]));
    }

    #[rstest]
    #[case(
        "!false",
        Unary(
            Operator::Logical(LogicalOp::Not),
            Box::new(Expression::Atom(Bool(false))),
        )
    )]
    #[case(
        "!true",
        Unary(
            Operator::Logical(LogicalOp::Not),
            Box::new(Expression::Atom(Bool(true))),
        )
    )]
    #[case(
        "!!true",
        Unary(
            Operator::Logical(LogicalOp::Not),
            Box::new(Unary(
                Operator::Logical(LogicalOp::Not),
                Box::new(Expression::Atom(Bool(true))),
            )),
        )
    )]
    fn unary_boolean(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case(
        "true && true",
        Binary(
            Box::new(Expression::Atom(Bool(true))),
            Operator::Logical(LogicalOp::And),
            Box::new(Expression::Atom(Bool(true))),
        )
    )]
    fn binary_boolean(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[test]
    fn test_parser_bool_unary_ops_repeated() {
        assert_parse_eq(
            "!!true",
            Unary(
                Operator::Logical(LogicalOp::Not),
                Box::new(Unary(
                    Operator::Logical(LogicalOp::Not),
                    Box::new(Expression::Atom(Bool(true))),
                )),
            ),
        );
    }

    /* TODO: this needs to be a slice
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
    */

    #[test]
    fn mixed_type_list() {
        assert_parse_eq(
            "['0', 1, 3.0]",
            List(vec![
                Expression::Atom(String("0".to_string())),
                Expression::Atom(Number(dec!(1))),
                Expression::Atom(Number(dec!(3.0))),
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
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Relation(RelationOp::In),
                Box::new(List(vec![Expression::Atom(Number(dec!(2)))])),
            ),
        );
    }

    #[test]
    fn integer_relations() {
        assert_parse_eq(
            "2 != 3",
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Relation(RelationOp::NotEquals),
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );
        assert_parse_eq(
            "2 == 3",
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Relation(RelationOp::Equals),
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );

        assert_parse_eq(
            "2 < 3",
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Relation(RelationOp::LessThan),
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );

        assert_parse_eq(
            "2 <= 3",
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Relation(RelationOp::LessThanEq),
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );
    }

    #[test]
    fn sbinary_product_expressions() {
        assert_parse_eq(
            "2 * 3",
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Arithmetic(ArithmeticOp::Multiply),
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );
    }

    #[test]
    fn test_parser_sum_expressions() {
        assert_parse_eq(
            "2 + 3",
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Arithmetic(ArithmeticOp::Add),
                Box::new(Expression::Atom(Number(dec!(3)))),
            ),
        );

        assert_parse_eq(
            "2 - -3",
            Binary(
                Box::new(Expression::Atom(Number(dec!(2)))),
                Operator::Arithmetic(ArithmeticOp::Subtract),
                Box::new(Expression::Atom(Number(dec!(-3)))),
            ),
        );
    }

    #[test]
    fn conditionals() {
        assert_parse_eq(
            "true && true",
            Binary(
                Box::new(Expression::Atom(Bool(true))),
                Operator::Logical(LogicalOp::And),
                Box::new(Expression::Atom(Bool(true))),
            ),
        );
        assert_parse_eq(
            "false || true",
            Binary(
                Box::new(Expression::Atom(Bool(false))),
                Operator::Logical(LogicalOp::Or),
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
            Binary(
                Box::new(Identifier("a".into())),
                Operator::Logical(LogicalOp::And),
                Box::new(Binary(
                    Box::new(Identifier("b".into())),
                    Operator::Relation(RelationOp::Equals),
                    Box::new(Expression::Atom(String("string".to_string()))),
                )),
            ),
        );
    }

    #[test]
    fn test_foobar() {
        println!("{:?}", parse("foo.bar.baz == 10 && size(requests) == 3"))
    }

    #[rstest]
    #[case("2021-01-01T", chrono::Utc.with_ymd_and_hms(2021, 1, 1, 0, 0, 0).unwrap().into())]
    #[case("2021-01-01T14:20:01", chrono::Utc.with_ymd_and_hms(2021, 1, 1, 14, 20, 1).unwrap().into())]
    #[case("2021-01-01T14:20:01-08:00", chrono::Utc.with_ymd_and_hms(2021, 1, 1, 22, 20, 1).unwrap().into())]
    fn test_dates(#[case] input: &str, #[case] expected: lexer::Atom) {
        assert_parse_eq(input, Atom(expected));
    }

    #[rstest]
    #[case("1h", Atom(chrono::Duration::hours(1).into()))]
    #[case("1m", Atom(chrono::Duration::minutes(1).into()))]
    #[case("1s", Atom(chrono::Duration::seconds(1).into()))]
    fn test_duration(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
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
    fn test_tag_set() {
        assert_parse_eq(
            "{ #foo_bar, #💕 }",
            TagSet(vec![
                Atom(HashTag("foo_bar".into())),
                Atom(HashTag("💕".into())),
            ]),
        )
    }
}
