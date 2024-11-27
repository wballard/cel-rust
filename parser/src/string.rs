use chumsky::prelude::*;

/// Parse the provided quoted string.
/// This function was adopted from [snailquote](https://docs.rs/snailquote/latest/snailquote/).
///
/// # Details
///
/// Parses a single or double quoted string and interprets escape sequences such as
/// '\n', '\r', '\'', etc.
///
/// " quoted strings allow escaping \"
/// ' quoted strings allow escaping \'
///
/// Supports raw strings prefixed with `r` or `R` in which case all escape sequences are ignored.///
///
/// The full set of supported escapes between quotes may be found below:
///
/// | Escape     | Code       | Description                              |
/// |------------|------------|------------------------------------------|
/// | \a         | 0x07       | Bell                                     |
/// | \b         | 0x08       | Backspace                                |
/// | \v         | 0x0B       | Vertical tab                             |
/// | \f         | 0x0C       | Form feed                                |
/// | \n         | 0x0A       | Newline                                  |
/// | \r         | 0x0D       | Carriage return                          |
/// | \t         | 0x09       | Tab                                      |
/// | \\         | 0x5C       | Backslash                                |
/// | \?         | 0x??       | Question mark                            |
/// | \"         | 0x22       | Double quote                             |
/// | \'         | 0x27       | Single quote                             |
/// | \`         | 0x60       | Backtick                                 |
/// | \xDD       | 0xDD       | Unicode character with hex code DD       |
/// | \uDDDD     | 0xDDDD     | Unicode character with hex code DDDD     |
/// | \UDDDDDDDD | 0xDDDDDDDD | Unicode character with hex code DDDDDDDD |
/// | \DDD       | 0DDD       | Unicode character with octal code DDD    |
///
/// # Example
///
/// ```
/// use cel_parser::string::parse_string;
/// use chumsky::Parser;
///
/// let parsed = parse_string().parse("\"Hello \\n World\"").unwrap();
/// assert_eq!(parsed, "Hello \n World");
///
/// let parsed = parse_string().parse("'Hello \\t World'").unwrap();
/// assert_eq!(parsed, "Hello \t World");
///
/// let parsed = parse_string().parse("r\"Hello \\n World\"").unwrap();
/// assert_eq!(parsed, "Hello \\n World");
///
/// let parsed = parse_string().parse("R'Hello \\t World'").unwrap();
/// assert_eq!(parsed, "Hello \\t World");
/// ```
pub fn parse_string<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> {
    choice((
        parse_single_quoted(),
        parse_double_quoted(),
        parse_single_quoted_raw(),
        parse_double_quoted_raw(),
    ))
}

/// Parse a unicode sequence, in forms:
///
/// * u{XXXX}, where XXXX is 1 to 6
/// * \xXX, where XX is 2 hexadecimal numerals
/// * \uXXXX, where XXXX is 4 hexadecimal numerals
/// * \UXXXXXXXX, where XXXXXXXX is 8 hexadecimal numerals
/// * \DDD where DDD is 1 to 3 octal digits
fn parse_unicode<'a>() -> impl Parser<'a, &'a str, char, extra::Err<Rich<'a, char>>> {
    let escape_dual = just("\\x").ignore_then(text::digits(16).exactly(2).to_slice());
    let escape_quad = just("\\u").ignore_then(text::digits(16).exactly(4).to_slice());
    let escape_octet = just("\\U").ignore_then(text::digits(16).exactly(8).to_slice());
    let bracketed =
        (text::digits(16).at_least(1).at_most(6).to_slice()).delimited_by(just("u{"), just("}"));
    let hex = choice((escape_dual, escape_quad, escape_octet, bracketed)).validate(
        |digits, e, emitter| {
            char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(|| {
                emitter.emit(Rich::custom(e.span(), "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        },
    );
    let octal = just("\\")
        .ignore_then(text::digits(8).exactly(3).to_slice())
        .validate(|digits, e, emitter| {
            char::from_u32(u32::from_str_radix(digits, 8).unwrap()).unwrap_or_else(|| {
                emitter.emit(Rich::custom(e.span(), "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });
    choice((hex, octal))
}

/// The classic C style \ escape characters
fn parse_escapes<'a>() -> impl Parser<'a, &'a str, char, extra::Err<Rich<'a, char>>> {
    just('\\').ignore_then(choice((
        just('\\').to('\\'),
        just('/').to('/'),
        just('"').to('"'),
        just('\'').to('\''),
        just('`').to('`'),
        just('?').to('?'),
        just('a').to('\x07'),
        just('b').to('\x08'),
        just('v').to('\x0B'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
    )))
}

fn parse_double_quoted<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> {
    choice((none_of("\"\\"), parse_escapes(), parse_unicode()))
        .repeated()
        .collect::<String>()
        .delimited_by(just('"'), just('"'))
}

fn parse_single_quoted<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> {
    choice((none_of("'\\"), parse_escapes(), parse_unicode()))
        .repeated()
        .collect::<String>()
        .delimited_by(just("'"), just("'"))
}

fn parse_double_quoted_raw<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> {
    choice((just("r"), just("R"))).ignore_then(
        choice((none_of("\"\\"), just("\\\"").to('"'), just("\\").to('\\')))
            .repeated()
            .collect::<String>()
            .delimited_by(just('"'), just('"')),
    )
}

fn parse_single_quoted_raw<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> {
    choice((just("r"), just("R"))).ignore_then(
        choice((none_of("'\\"), just("\\'").to('\''), just("\\").to('\\')))
            .repeated()
            .collect::<String>()
            .delimited_by(just('\''), just('\'')),
    )
}

#[cfg(test)]
mod tests {

    use super::*;
    use rstest::rstest;

    #[test]
    fn test_parse_unicode() {
        assert_eq!(parse_unicode().parse("\\u270C").unwrap(), '✌');
        assert_eq!(parse_unicode().parse("\\xAC").unwrap(), '¬');
        assert_eq!(parse_unicode().parse("\\U0001f431").unwrap(), '🐱');
        assert_eq!(parse_unicode().parse("u{1F600}").unwrap(), '😀');
        assert_eq!(parse_unicode().parse("\\254").unwrap(), '¬');
    }

    #[test]
    fn test_parse_escape() {
        assert_eq!(parse_escapes().parse("\\a").unwrap(), '\u{07}');
    }
    #[test]
    fn test_parse_unicode_fail() {
        assert_eq!(
            parse_unicode()
                .parse("\\u27ZC")
                .into_errors()
                .first()
                .unwrap()
                .to_string(),
            "found Z expected something else"
        );
    }

    #[rstest]
    #[case("\"Hello\"", String::from("Hello"))]
    #[case("\"Hello \\a\"", String::from("Hello \u{07}"))]
    #[case("\"Hello \\b\"", String::from("Hello \u{08}"))]
    #[case("\"Hello \\v\"", String::from("Hello \u{0b}"))]
    #[case("\"Hello \\f\"", String::from("Hello \u{0c}"))]
    #[case("\"Hello \\n\"", String::from("Hello \u{0a}"))]
    #[case("\"Hello \\r\"", String::from("Hello \u{0d}"))]
    #[case("\"Hello \\t\"", String::from("Hello \u{09}"))]
    #[case("\"Hello \\\\\"", String::from("Hello \\"))]
    #[case("\"Hello \\?\"", String::from("Hello ?"))]
    #[case("\"Hello \\\"\"", String::from("Hello \""))]
    #[case("\"Hello \\'\"", String::from("Hello '"))]
    #[case("\"Hello \\`\"", String::from("Hello `"))]
    #[case("\"Hello \\x20 \"", String::from("Hello   "))]
    #[case("\"Hello \\x60\"", String::from("Hello `"))]
    #[case("\"Hello \\u270c\"", String::from("Hello ✌"))]
    #[case("\"Hello \\U0001f431\"", String::from("Hello 🐱"))]
    fn double_quotes_interprets_escapes(#[case] script: &str, #[case] expected: String) {
        let result = parse_string().parse(script);
        assert_eq!(result.unwrap(), expected)
    }

    #[rstest]
    #[case("'Hello'", String::from("Hello"))]
    #[case("'Hello \\a'", String::from("Hello \u{07}"))]
    #[case("'Hello \\b'", String::from("Hello \u{08}"))]
    #[case("'Hello \\v'", String::from("Hello \u{0b}"))]
    #[case("'Hello \\f'", String::from("Hello \u{0c}"))]
    #[case("'Hello \\n'", String::from("Hello \u{0a}"))]
    #[case("'Hello \\r'", String::from("Hello \u{0d}"))]
    #[case("'Hello \\t'", String::from("Hello \u{09}"))]
    #[case("'Hello \\\\'", String::from("Hello \\"))]
    #[case("'Hello \\?'", String::from("Hello ?"))]
    #[case("'Hello \\\"'", String::from("Hello \""))]
    #[case("'Hello \\''", String::from("Hello '"))]
    #[case("'Hello \\`'", String::from("Hello `"))]
    #[case("'Hello \\x20 '", String::from("Hello   "))]
    #[case("'Hello \\x60'", String::from("Hello `"))]
    #[case("'Hello \\u270c'", String::from("Hello ✌"))]
    #[case("'Hello \\U0001f431'", String::from("Hello 🐱"))]
    fn single_quotes_interprets_escapes(#[case] script: &str, #[case] expected: String) {
        let result = parse_string().parse(script);
        assert_eq!(result.unwrap(), expected)
    }

    #[rstest]
    #[case(
        "r\"Hello \\a \\\" ' \\' \\U0001f431 \"",
        String::from("Hello \\a \" ' \\' \\U0001f431 ")
    )]
    #[case(
        "R\"Hello \\a \\\" ' \\' \\U0001f431 \"",
        String::from("Hello \\a \" ' \\' \\U0001f431 ")
    )]
    fn raw_double_quotes(#[case] script: &str, #[case] expected: String) {
        let result = parse_string().parse(script);
        assert_eq!(result.unwrap(), expected)
    }
    #[rstest]
    #[case(
        "r'Hello \\a \\\" \" \\' \\U0001f431 '",
        String::from("Hello \\a \\\" \" ' \\U0001f431 ")
    )]
    #[case(
        "R'Hello \\a \\\" \" \\' \\U0001f431 '",
        String::from("Hello \\a \\\" \" ' \\U0001f431 ")
    )]
    fn raw_single_quotes(#[case] script: &str, #[case] expected: String) {
        let result = parse_string().parse(script);
        assert_eq!(result.unwrap(), expected)
    }
}
