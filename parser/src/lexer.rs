pub use crate::atoms::*;
pub use crate::operators::*;
use chumsky::extra;
use chumsky::input::BorrowInput;
use chumsky::input::ValueInput;
use chumsky::prelude::*;

use std::fmt::*;

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
            just(',').map(|_| Token::Separator),
            parse_op().boxed().map(Token::Operator),
            parse_atom().boxed().map(Token::Atom),
            parse_identifier().boxed().map(Token::Identifier),
        ))
        .map_with(|token, e| (token, e.span()))
        .padded()
    })
    .repeated()
    .collect()
}

// Convert a slice of tokens into a spanned value input.
pub fn make_input<'src>(
    eoi: SimpleSpan,
    toks: &'src [Spanned<Token>],
) -> impl BorrowInput<'src, Token = Token, Span = SimpleSpan> {
    toks.spanned(eoi)
}
