use chumsky::prelude::*;
use std::fmt;
use ulid::Ulid;

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

impl From<&str> for HashTag {
    fn from(s: &str) -> Self {
        if s.starts_with("#") {
            HashTag(s.strip_prefix("#").unwrap().to_string())
        } else {
            HashTag(s.to_string())
        }
    }
}

impl From<String> for HashTag {
    fn from(s: String) -> Self {
        if s.starts_with("#") {
            HashTag(s.strip_prefix("#").unwrap().to_string())
        } else {
            HashTag(s)
        }
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
    let body_pattern = r"\p{XID_Start}[\p{XID_Continue}\p{Emoji}]*";
    just("#")
        .ignore_then(regex(body_pattern))
        .map(|s: &str| s.into())
}
