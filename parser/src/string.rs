//! This example shows an example of how to parse an escaped string. The
//! rules for the string are similar to JSON and rust. A string is:
//!
//! - Enclosed by double quotes
//! - Can contain any raw unescaped code point besides \ and "
//! - Matches the following escape sequences: \b, \f, \n, \r, \t, \", \\, \/
//! - Matches code points like Rust: \u{XXXX}, where XXXX can be up to 6
//!   hex characters
//! - an escape followed by whitespace consumes all whitespace between the
//!   escape and the next non-whitespace character

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while_m_n};
use nom::character::complete::char;
//use nom::character::streaming::{char, hex_digit1, multispace1};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{FromExternalError, ParseError};
use nom::multi::fold;
use nom::sequence::{delimited, preceded};
use nom::{AsChar, IResult, Parser};

// parser combinators are constructed from the bottom up:
// first we write parsers for the smallest elements (escaped characters),
// then combine them into larger parsers.

/// Parse a unicode sequence, in forms:
///
/// * u{XXXX}, where XXXX is 1 to 6
/// * \xXX, where XX is 2 hexadecimal numerals
/// * \uXXXX, where XXXX is 4 hexadecimal numerals
/// * \UXXXXXXXX, where XXXXXXXX is 8 hexadecimal numerals
/// * \DDD where DDD is 1 to 3 octal digits
fn parse_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let parse_delimited_hex = preceded(
        char('u'),
        delimited(
            char('{'),
            take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit()),
            char('}'),
        ),
    );

    let parse_prefixed_hex_pair = preceded(
        tag("\\x"),
        take_while_m_n(1, 2, |c: char| c.is_ascii_hexdigit()),
    );
    let parse_prefixed_u_quad = preceded(
        tag("\\u"),
        take_while_m_n(4, 4, |c: char| c.is_ascii_hexdigit()),
    );
    let parse_prefixed_u_sextet = preceded(
        tag("\\U"),
        take_while_m_n(8, 8, |c: char| c.is_ascii_hexdigit()),
    );
    let parse_octal_triplet = preceded(tag("\\"), take_while_m_n(3, 3, |c: char| c.is_oct_digit()));

    let hex_formats = alt((
        parse_prefixed_hex_pair,
        parse_prefixed_u_quad,
        parse_prefixed_u_sextet,
        parse_delimited_hex,
    ));

    let parse_u32 = alt((
        map_res(hex_formats, move |hex| u32::from_str_radix(hex, 16)),
        map_res(parse_octal_triplet, move |oct| u32::from_str_radix(oct, 8)),
    ));

    map_opt(parse_u32, std::char::from_u32).parse(input)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse_unicode() {
        assert_eq!(parse_unicode::<()>("u{1F600}"), Ok(("", '😀')));
        assert_eq!(parse_unicode::<()>("\\u270C"), Ok(("", '✌')));
        assert_eq!(parse_unicode::<()>("\\xAC"), Ok(("", '¬')));
        assert_eq!(parse_unicode::<()>("\\U0001f431"), Ok(("", '🐱')));
        assert_eq!(parse_unicode::<()>("\\254"), Ok(("", '¬')));
    }
}
