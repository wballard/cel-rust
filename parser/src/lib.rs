use chumsky::input::BorrowInput;
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

/// We want to forgive trailing commas to have a friendly syntax.
fn trim_trailing_separator(ts: &Vec<(Token, SimpleSpan)>) -> &[(Token, SimpleSpan)] {
    match ts.last() {
        Some((Token::Separator, _)) => {
            let trim = ts.split_last().unwrap().1;
            trim
        }
        _ => ts,
    }
}

pub fn parse_expression<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Expression, extra::Err<Rich<'src, Token>>>
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
    M: Fn(SimpleSpan, &'src [Spanned<Token>]) -> I + 'src + Copy,
{
    let atom = select_ref! { Token::Atom(x) => Expression::Atom(x.clone()) };
    let identifier = select_ref! { Token::Identifier(x) => {
        Expression::Identifier(x.clone())
    }};
    let atoms = choice((identifier, atom));

    // relation operators with the same level of precedence
    let relations = choice((
        just(Token::Operator(Operator::Relation(RelationOp::Equals))),
        just(Token::Operator(Operator::Relation(RelationOp::NotEquals))),
        just(Token::Operator(Operator::Relation(RelationOp::LessThan))),
        just(Token::Operator(Operator::Relation(RelationOp::LessThanEq))),
        just(Token::Operator(Operator::Relation(RelationOp::GreaterThan))),
        just(Token::Operator(Operator::Relation(
            RelationOp::GreaterThanEq,
        ))),
    ))
    .map(|e| match e {
        Token::Operator(Operator::Relation(op)) => op,
        _ => unreachable!(),
    });
    // different arithmetic precedence levels
    let arithmetic_high = choice((
        just(Token::Operator(Operator::Arithmetic(
            ArithmeticOp::Multiply,
        ))),
        just(Token::Operator(Operator::Arithmetic(ArithmeticOp::Divide))),
        just(Token::Operator(Operator::Arithmetic(ArithmeticOp::Modulus))),
    ))
    .map(|e| match e {
        Token::Operator(Operator::Arithmetic(op)) => op,
        _ => unreachable!(),
    });
    let arithmetic_low = choice((
        just(Token::Operator(Operator::Arithmetic(ArithmeticOp::Add))),
        just(Token::Operator(Operator::Arithmetic(
            ArithmeticOp::Subtract,
        ))),
    ))
    .map(|e| match e {
        Token::Operator(Operator::Arithmetic(op)) => op,
        _ => unreachable!(),
    });

    recursive(|expression| {
        choice((
            // calls are a function name followed by a list of arguments
            identifier
                .then(expression.clone())
                .map(|(left, right)| match left {
                    Expression::Identifier(name) => match right {
                        // identifier and a tuple? -- that's a function call
                        Expression::Tuple(args) => Expression::FunctionCall(name, None, args),

                        _ => {
                            print!("hi {:?}", right);
                            unreachable!()
                        }
                    },
                    _ => unreachable!(),
                }),
            // simple atoms
            atoms,
            // empty nests
            just(Token::Parens(vec![])).map(|_| Expression::Tuple(vec![])),
            just(Token::Braces(vec![])).map(|_| Expression::Set(vec![])),
            just(Token::Brackets(vec![])).map(|_| Expression::List(vec![])),
            // compound nests
            // picking out the nested multiples and use them as members
            // empty expression ... empty nest
            // default to a single element nest for singulars
            expression
                .clone()
                .nested_in(select_ref! { Token::Brackets(ts) = e => {
                    make_input(e.span(), trim_trailing_separator(ts))
                }})
                .map(|nested| match nested {
                    Expression::Multiple(exprs) => Expression::List(exprs),
                    any => Expression::List(vec![any]),
                }),
            expression
                .clone()
                .nested_in(select_ref! { Token::Parens(ts) = e => {
                    make_input(e.span(), trim_trailing_separator(ts))
                }})
                .map(|nested| match nested {
                    Expression::Multiple(exprs) => Expression::Tuple(exprs),
                    any => Expression::Tuple(vec![any]),
                }),
            expression
                .clone()
                .nested_in(select_ref! { Token::Braces(ts) = e => {
                    make_input(e.span(), trim_trailing_separator(ts))
                }})
                .map(|nested| match nested {
                    Expression::Multiple(exprs) => Expression::Set(exprs),
                    any => Expression::Set(vec![any]),
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
            // arithmetic operators
            infix(
                left(390),
                just(Token::Operator(Operator::Arithmetic(
                    ArithmeticOp::Exponent,
                ))),
                |left, op, right| {
                    Expression::Binary(
                        Box::new(left),
                        Operator::Arithmetic(ArithmeticOp::Exponent),
                        Box::new(right),
                    )
                },
            ),
            infix(left(380), arithmetic_high, |left, op, right| {
                Expression::Binary(Box::new(left), Operator::Arithmetic(op), Box::new(right))
            }),
            infix(left(370), arithmetic_low, |left, op, right| {
                Expression::Binary(Box::new(left), Operator::Arithmetic(op), Box::new(right))
            }),
            // relation operators
            infix(
                left(290),
                just(Token::Operator(Operator::Relation(RelationOp::GetMember))),
                |left, right| {
                    Expression::Binary(
                        Box::new(left),
                        Operator::Relation(RelationOp::GetMember),
                        Box::new(right),
                    )
                },
            ),
            infix(
                left(280),
                just(Token::Operator(Operator::Relation(RelationOp::In))),
                |left, right| {
                    Expression::Binary(
                        Box::new(left),
                        Operator::Relation(RelationOp::In),
                        Box::new(right),
                    )
                },
            ),
            infix(left(270), relations, |left, op, right| {
                Expression::Binary(Box::new(left), Operator::Relation(op), Box::new(right))
            }),
            // logical operators
            prefix(
                100,
                just(Token::Operator(Operator::Logical(LogicalOp::Not))),
                |right| Expression::Unary(Operator::Logical(LogicalOp::Not), Box::new(right)),
            ),
            infix(
                left(90),
                just(Token::Operator(Operator::Logical(LogicalOp::Xor))),
                |left, right| {
                    Expression::Binary(
                        Box::new(left),
                        Operator::Logical(LogicalOp::Xor),
                        Box::new(right),
                    )
                },
            ),
            infix(
                left(80),
                just(Token::Operator(Operator::Logical(LogicalOp::And))),
                |left, right| {
                    Expression::Binary(
                        Box::new(left),
                        Operator::Logical(LogicalOp::And),
                        Box::new(right),
                    )
                },
            ),
            infix(
                left(70),
                just(Token::Operator(Operator::Logical(LogicalOp::Or))),
                |left, right| {
                    Expression::Binary(
                        Box::new(left),
                        Operator::Logical(LogicalOp::Or),
                        Box::new(right),
                    )
                },
            ),
        ))
    })

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
    #[case("()", Tuple(vec![
    ]))]
    #[case("(1)", Tuple(vec![
        Atom(Number(dec!(1))),
    ]))]
    #[case("(1, 2, 3)", Tuple(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    #[case("(1, 2, 3,)", Tuple(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    fn tuple(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("{}", Set(vec![
    ]))]
    #[case("{1}", Set(vec![
        Atom(Number(dec!(1))),
    ]))]
    #[case("{1, 2, 3}", Set(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    #[case("{1, 2, 3,}", Set(vec![
        Atom(Number(dec!(1))),
        Atom(Number(dec!(2))),
        Atom(Number(dec!(3))),
    ]))]
    fn set(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[test]
    fn to_string() {
        assert_parse_eq(
            "[1, 2, 3].string()",
            Binary(
                Box::new(List(vec![
                    Atom(Number(dec!(1))),
                    Atom(Number(dec!(2))),
                    Atom(Number(dec!(3))),
                ])),
                Operator::Relation(RelationOp::GetMember),
                Box::new(FunctionCall("string".into(), None, vec![])),
            ),
        )
    }

    #[test]
    fn map_list_constant() {
        assert_parse_eq(
            "[1, 2, 3].map(x, x * 2)",
            Binary(
                Box::new(List(vec![
                    Atom(Number(dec!(1))),
                    Atom(Number(dec!(2))),
                    Atom(Number(dec!(3))),
                ])),
                Operator::Relation(RelationOp::GetMember),
                Box::new(FunctionCall(
                    "map".into(),
                    None,
                    vec![
                        Identifier("x".into()),
                        Binary(
                            Box::new(Identifier("x".into())),
                            Operator::Arithmetic(ArithmeticOp::Multiply),
                            Box::new(Atom(Number(dec!(2)))),
                        ),
                    ],
                )),
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

    #[rstest]
    #[case("a()", FunctionCall("a".into(), None, vec![]))]
    #[case("a(1)", FunctionCall("a".into(), None, vec![Atom(Number(dec!(1)))]))]
    #[case("a(x)", FunctionCall("a".into(), None, vec![Identifier("x".into())]))]
    fn function_call(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
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
    #[case(
        "true || true",
        Binary(
            Box::new(Expression::Atom(Bool(true))),
            Operator::Logical(LogicalOp::Or),
            Box::new(Expression::Atom(Bool(true))),
        )
    )]
    #[case(
        "true ^^ true",
        Binary(
            Box::new(Expression::Atom(Bool(true))),
            Operator::Logical(LogicalOp::Xor),
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

    #[rstest]
    #[case("2 in [2]", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::In),
        Box::new(List(vec![Expression::Atom(Number(dec!(2)))])),
    ))]
    #[case("2 in {2}", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::In),
        Box::new(Set(vec![Expression::Atom(Number(dec!(2)))])),
    ))]
    #[case("2 in (2)", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::In),
        Box::new(Tuple(vec![Expression::Atom(Number(dec!(2)))])),
    ))]
    fn in_relation(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
    }

    #[rstest]
    #[case("2 != 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::NotEquals),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 == 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::Equals),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 < 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::LessThan),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 <= 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::LessThanEq),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 > 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::GreaterThan),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 >= 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Relation(RelationOp::GreaterThanEq),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    fn integer_relations(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
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

    #[rstest]
    #[case("2 + 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Arithmetic(ArithmeticOp::Add),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 - 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Arithmetic(ArithmeticOp::Subtract),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 * 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Arithmetic(ArithmeticOp::Multiply),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 / 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Arithmetic(ArithmeticOp::Divide),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 % 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Arithmetic(ArithmeticOp::Modulus),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    #[case("2 ^ 3", Binary(
        Box::new(Expression::Atom(Number(dec!(2)))),
        Operator::Arithmetic(ArithmeticOp::Exponent),
        Box::new(Expression::Atom(Number(dec!(3)))),
    ))]
    fn integer_arithmetic(#[case] input: &str, #[case] expected: Expression) {
        assert_parse_eq(input, expected);
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
            Set(vec![
                Atom(HashTag("foo_bar".into())),
                Atom(HashTag("💕".into())),
            ]),
        )
    }
}
