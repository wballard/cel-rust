use chumsky::prelude::*;
use chumsky::Parser;
use std::fmt;
use ulid::Ulid;

use crate::Dispatchable;

/// Parses a ULID (Universally Unique Lexicographically Sortable Identifier) from a string slice.
///
/// The ULID consists of 26 characters from the set `[0123456789ABCDEFGHJKMNPQRSTVWXYZ]`
/// -- Crockford's Base32 encoding without the characters `ILUO`.
/// The parser expects the input string to start with an ampersand (`&`) followed by the ULID.
///
/// # Example
///
/// ```rust
/// use cel_parser::identifiers::parse_ulid;
/// use chumsky::Parser;
/// use ulid::Ulid;
///
/// let ulid = parse_ulid().parse("&01ARZ3NDEKTSV4RRFFQ69G5FAV").unwrap();
/// assert_eq!(ulid.to_string(), "01ARZ3NDEKTSV4RRFFQ69G5FAV");
/// ```
pub fn parse_ulid<'a>() -> impl Parser<'a, &'a str, Ulid, extra::Err<Rich<'a, char>>> {
    let pattern = r"[0123456789ABCDEFGHJKMNPQRSTVWXYZ]{26}";
    just("&")
        .ignore_then(regex(pattern))
        .map(|s| Ulid::from_string(s).unwrap())
}

/// A Hashtag is a string used to add metadata.
///
/// The first character in a [Unicode Identifier](https://www.unicode.org/reports/tr31/#Default_Identifier_Syntax)
/// Following characters can be any Unicode Identifier character or emoji.
///
/// The hashtag is stored in memory without the leading `#` character.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
pub struct HashTag(String);

impl AsRef<str> for HashTag {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<&str> for HashTag {
    fn from(s: &str) -> Self {
        if s.starts_with("#") {
            HashTag(s.strip_prefix("#").unwrap().to_string())
        } else {
            HashTag(s.to_string())
        }
    }
}

impl From<Identifier> for HashTag {
    fn from(id: Identifier) -> Self {
        HashTag(id.0)
    }
}

impl fmt::Display for HashTag {
    /// Formats the hashtag as a string with the classic '#' prefix.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// Parses a hashtag from a string slice.
///
/// The parser expects the input string to start with a hash (`#`) followed by the hashtag.
///
/// The first character in a [Unicode Identifier](https://www.unicode.org/reports/tr31/#Default_Identifier_Syntax).
///
/// Subsequence characters include any Unicode Identifier character or emoji.
///
/// # Example
///
/// ```rust
/// use cel_parser::identifiers::{parse_hashtag, HashTag};
/// use chumsky::Parser;
///
/// let hashtag = parse_hashtag().parse("#rust_🦀").unwrap();
/// assert_eq!(hashtag, "rust_🦀".into());
///
/// assert_eq!(hashtag.to_string(), "#rust_🦀");
/// ```
///
pub fn parse_hashtag<'a>() -> impl Parser<'a, &'a str, HashTag, extra::Err<Rich<'a, char>>> {
    just("#").ignore_then(parse_identifier()).map(|s| s.into())
}

/// Parses the constant idenfitiers for `true` and `false`.
///
/// # Example
/// ```rust
/// use cel_parser::identifiers::parse_bool;
/// use chumsky::Parser;
///
/// let true_value = parse_bool().parse("true").unwrap();
/// assert_eq!(true_value, true);
///
/// let false_value = parse_bool().parse("false").unwrap();
/// assert_eq!(false_value, false);
/// ```
pub fn parse_bool<'a>() -> impl Parser<'a, &'a str, bool, extra::Err<Rich<'a, char>>> {
    let true_parser = just("true").map(|_| true);
    let false_parser = just("false").map(|_| false);
    true_parser.or(false_parser)
}

/// Identifiers are used to name things.
///
/// The first character in a [Unicode Identifier](https://www.unicode.org/reports/tr31/#Default_Identifier_Syntax).
///
/// Subsequence characters include any Unicode Identifier character or emoji.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Default)]
pub struct Identifier(String);

impl Dispatchable for Identifier {}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Identifier(s.to_string())
    }
}

impl From<&Identifier> for Identifier {
    fn from(id: &Identifier) -> Self {
        Identifier(id.0.clone())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&Identifier> for String {
    fn from(value: &Identifier) -> Self {
        value.0.to_string()
    }
}

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.0.clone()
    }
}

/// Parses an identifier from a string slice.
///
/// Identifiers include:
/// * Unicode Identifier characters
/// * emoji
/// * _
///
/// *but not digits or punctuation*.
///
/// Digits are excluded to allow for `units` parsing such as `15m` to be parsed
/// as a `Measure` instead of an `Identifier`.
///
/// Example:
///
/// ```rust
/// use cel_parser::identifiers::{parse_identifier, Identifier};
/// use chumsky::Parser;
///
/// let identifier_with_emoji = parse_identifier().parse("rust_🦀").unwrap();
/// assert_eq!(identifier_with_emoji, "rust_🦀".into());
///
///
/// ```
///
pub fn parse_identifier<'a>() -> impl Parser<'a, &'a str, Identifier, extra::Err<Rich<'a, char>>> {
    // start with a Unicode Identifier Start character or an emoji
    // but not any of our punctuation characters that get used as sigils and operators
    let body_pattern = r"[\p{ID_Start}\p{Emoji}_&&[^\d]][\p{XID_Continue}\p{Emoji}_&&[^\d]]*";
    regex(body_pattern).map(|s: &str| s.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("rust_🦀", "rust_🦀".into())]
    #[case("rust_lang", "rust_lang".into())]
    #[case("🦃", "🦃".into())]
    #[case("_🦃_", "_🦃_".into())]
    fn identifier(#[case] input: &str, #[case] expected: Identifier) {
        let result = parse_identifier().parse(input).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn identifier_with_digits_fails() {
        let result = parse_identifier().parse("rust123");
        assert!(result.has_errors());
    }
}
