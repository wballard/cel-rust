pub use crate::atoms::*;
pub use crate::operators::*;
use chumsky::extra;
use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::Parser;
use std::fmt::{Display, Formatter};

/// Keep track of a token with its source span.
pub type Spanned<T> = (T, SimpleSpan);

/// Turn the raw text into workable tokens.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Atom(Atom),
    Parens(Vec<Spanned<Token>>),
    Brackets(Vec<Spanned<Token>>),
    Braces(Vec<Spanned<Token>>),
    Separator,
    Operator(Operator),
    Identifier(Identifier),
    Question,
    Colon,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Atom(atom) => write!(f, "{}", atom),
            Token::Separator => write!(f, ","),
            Token::Operator(op) => write!(f, "{:?}", op),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::Question => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::Parens(tokens) => {
                write!(f, "(")?;
                for token in tokens {
                    write!(f, "{}", token.0)?;
                }
                write!(f, ")")
            }
            Token::Brackets(tokens) => {
                write!(f, "[")?;
                for token in tokens {
                    write!(f, "{}", token.0)?;
                }
                write!(f, "]")
            }
            Token::Braces(tokens) => {
                write!(f, "{{")?;
                for token in tokens {
                    write!(f, "{}", token.0)?;
                }
                write!(f, "}}")
            }
        }
    }
}

/// Parse text into a sequence of tokens, consuming whitespace.
///
/// This is not the full AST, but a structured representation of the input text.
pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> {
    recursive(|token| {
        choice((
            // nesting expressions
            token
                .clone()
                .repeated()
                .collect()
                .delimited_by(just('('), just(')'))
                .labelled("parens")
                .as_context()
                .map(Token::Parens),
            token
                .clone()
                .repeated()
                .collect()
                .delimited_by(just('['), just(']'))
                .labelled("brackets")
                .as_context()
                .map(Token::Brackets),
            token
                .clone()
                .repeated()
                .collect()
                .delimited_by(just('{'), just('}'))
                .labelled("braces")
                .as_context()
                .map(Token::Braces),
            // the basic atoms
            parse_atom().boxed().map(Token::Atom),
            just(',').map(|_| Token::Separator),
            just('?').map(|_| Token::Question),
            just(':').map(|_| Token::Colon),
            parse_op().boxed().map(Token::Operator),
            parse_identifier().boxed().map(Token::Identifier),
        ))
        .map_with(|token, e| (token, e.span()))
        .padded()
    })
    .repeated()
    .collect()
}

// Convert a slice of tokens into a spanned value input.
pub fn make_input(
    eoi: SimpleSpan,
    toks: &[Spanned<Token>],
) -> impl BorrowInput<Token = Token, Span = SimpleSpan> {
    toks.spanned(eoi)
}
#[cfg(test)]
mod tests {
    use super::*;
    use rust_decimal_macros::dec;

    #[test]
    fn empty_tuple() {
        let input = "()";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::Parens(inner_tokens) => assert!(inner_tokens.is_empty()),
            _ => panic!("Expected Token::Parens"),
        }
    }

    #[test]
    fn empty_brackets() {
        let input = "[]";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::Brackets(inner_tokens) => assert!(inner_tokens.is_empty()),
            _ => panic!("Expected Token::Brackets"),
        }
    }

    #[test]
    fn empty_braces() {
        let input = "{}";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::Braces(inner_tokens) => assert!(inner_tokens.is_empty()),
            _ => panic!("Expected Token::Braces"),
        }
    }

    #[test]
    fn separator() {
        let input = ",";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::Separator => (),
            _ => panic!("Expected Token::Separator"),
        }
    }

    #[test]
    fn identifier_with_parens() {
        let input = "foo()";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 2);
        match &tokens[0].0 {
            Token::Identifier(id) => assert_eq!(id.to_string(), "foo"),
            _ => panic!("Expected Token::Identifier"),
        }
        match &tokens[1].0 {
            Token::Parens(inner_tokens) => assert!(inner_tokens.is_empty()),
            _ => panic!("Expected Token::Parens"),
        }
    }

    #[test]
    fn member_call() {
        let input = "foo.goo()";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(
            tokens,
            vec![
                (Token::Identifier("foo".into()), SimpleSpan::new(0, 3)),
                (
                    Token::Operator(Operator::Relation(RelationOp::GetMember)),
                    SimpleSpan::new(3, 4)
                ),
                (Token::Identifier("goo".into()), SimpleSpan::new(4, 7)),
                (Token::Parens(vec![]), SimpleSpan::new(7, 9)),
            ]
        );
    }

    #[test]
    fn indexer() {
        let input = "foo[0]";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(
            tokens,
            vec![
                (Token::Identifier("foo".into()), SimpleSpan::new(0, 3)),
                (
                    Token::Brackets(vec![(
                        Token::Atom(Atom::Number(dec!(0))),
                        SimpleSpan::new(4, 5)
                    )]),
                    SimpleSpan::new(3, 6)
                ),
            ]
        );
    }

    #[test]
    fn indexer_range() {
        let input = "foo[0..1]";
        let result = lexer().parse(input).into_result();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        assert_eq!(
            tokens,
            vec![
                (Token::Identifier("foo".into()), SimpleSpan::new(0, 3)),
                (
                    Token::Brackets(vec![
                        (Token::Atom(Atom::Number(dec!(0))), SimpleSpan::new(4, 5)),
                        (
                            Token::Operator(Operator::Relation(RelationOp::Range)),
                            SimpleSpan::new(5, 7)
                        ),
                        (Token::Atom(Atom::Number(dec!(1))), SimpleSpan::new(7, 8)),
                    ]),
                    SimpleSpan::new(3, 9)
                ),
            ]
        );
    }
}
