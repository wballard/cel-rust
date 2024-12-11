use chumsky::prelude::*;
use chumsky::Parser;
use regex::Regex;

/// Parse a regular expression surrounded by slashes.
pub fn parse_regex<'a>() -> impl Parser<'a, &'a str, Regex, extra::Err<Rich<'a, char>>> {
    choice((parse_escapes(), none_of(r"/")))
        .repeated()
        .collect::<String>()
        .try_map(|s, span| match Regex::new(&s) {
            Ok(re) => Ok(re),
            Err(e) => Err(Rich::custom(span, e.to_string())),
        })
        .delimited_by(just('/'), just('/'))
}

/// Escapes for the / separator.
fn parse_escapes<'a>() -> impl Parser<'a, &'a str, char, extra::Err<Rich<'a, char>>> {
    just('\\').ignore_then(choice((just('/').to('/'),)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("/[a-z]+/", "[a-z]+")]
    #[case(r"/\d+/", "\\d+")]
    #[case(r"/\w{3}/", "\\w{3}")]
    fn regex(#[case] input: &str, #[case] expected: &str) {
        let result = parse_regex().parse(input).unwrap();
        assert_eq!(result.as_str(), expected);
    }
}
