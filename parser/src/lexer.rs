pub use crate::atoms::*;
pub use crate::operators::*;
use chumsky::extra;
use chumsky::prelude::*;

use std::fmt::*;

/// Turn the raw text into workable tokens.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Atom(Atom),
    Parens(Vec<Token>),
    Brackets(Vec<Token>),
    Braces(Vec<Token>),
    Separator,
    Operator(Operator),
    Identifier(Identifier),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Atom(atom) => write!(f, "{}", atom),
            Token::Separator => write!(f, ","),
            Token::Operator(op) => write!(f, "{:?}", op),
            Token::Identifier(id) => write!(f, "{}", id),
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

/// Parse text into a sequence of tokens, consuming whitespace.
///
/// This is not the full AST, but a structured representation of the input text.
pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Token>, extra::Err<Rich<'a, char>>> {
    recursive(|token| {
        choice((
            // nesting expressions
            token
                .clone()
                .repeated()
                .collect()
                .delimited_by(just('('), just(')'))
                .map(Token::Parens),
            token
                .clone()
                .repeated()
                .collect()
                .delimited_by(just('['), just(']'))
                .map(Token::Brackets),
            // the basic atoms
            just(',').map(|_| Token::Separator),
            parse_op().boxed().map(Token::Operator),
            parse_atom().boxed().map(Token::Atom),
            parse_identifier().boxed().map(Token::Identifier),
        ))
        .padded()
    })
    .repeated()
    .collect()
}
