pub use crate::datetime::*;
pub use crate::identifiers::*;
pub use crate::numbers::*;
use crate::parse_regex;
pub use crate::string::*;
pub use crate::units::*;
use chumsky::prelude::*;
use std::fmt::*;

/// Wrap up a regex to implement Eq, Display, Hash.
#[derive(Debug, Clone)]
pub struct RegexWrapper(pub regex::Regex);

impl PartialEq for RegexWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl Eq for RegexWrapper {}

impl std::hash::Hash for RegexWrapper {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state)
    }
}

impl Display for RegexWrapper {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "/{}/", self.0)
    }
}

impl From<regex::Regex> for RegexWrapper {
    fn from(regex: regex::Regex) -> Self {
        RegexWrapper(regex)
    }
}

impl From<RegexWrapper> for String {
    fn from(regex: RegexWrapper) -> Self {
        regex.0.as_str().to_string()
    }
}

impl From<regex::Regex> for Atom {
    fn from(regex: regex::Regex) -> Self {
        Atom::Regex(RegexWrapper::from(regex))
    }
}

/// Represents an atomic value in an expression.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Atom {
    Number(rust_decimal::Decimal),
    String(String),
    Bool(bool),
    Null,
    Ulid(ulid::Ulid),
    DateTime(chrono::DateTime<chrono::Utc>),
    Duration(chrono::Duration),
    HashTag(HashTag),
    Regex(RegexWrapper),
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Atom::Number(n) => write!(f, "{}", n),
            Atom::String(s) => write!(f, "{}", s),
            Atom::Bool(b) => write!(f, "{}", b),
            Atom::Null => write!(f, "null"),
            Atom::Ulid(u) => write!(f, "{}", u),
            Atom::DateTime(d) => write!(f, "{}", d),
            Atom::Duration(d) => write!(f, "{}s", d.num_seconds()),
            Atom::HashTag(h) => write!(f, "{}", h),
            Atom::Regex(r) => write!(f, "{}", r),
        }
    }
}

impl From<ulid::Ulid> for Atom {
    fn from(ulid: ulid::Ulid) -> Self {
        Atom::Ulid(ulid)
    }
}

impl From<rust_decimal::Decimal> for Atom {
    fn from(decimal: rust_decimal::Decimal) -> Self {
        Atom::Number(decimal)
    }
}

impl From<chrono::DateTime<chrono::Utc>> for Atom {
    fn from(datetime: chrono::DateTime<chrono::Utc>) -> Self {
        Atom::DateTime(datetime)
    }
}

impl From<chrono::Duration> for Atom {
    fn from(duration: chrono::Duration) -> Self {
        Atom::Duration(duration)
    }
}

impl From<bool> for Atom {
    fn from(b: bool) -> Self {
        Atom::Bool(b)
    }
}

impl From<String> for Atom {
    fn from(s: String) -> Self {
        Atom::String(s)
    }
}

impl From<&str> for Atom {
    fn from(s: &str) -> Self {
        Atom::String(s.to_string())
    }
}

impl From<HashTag> for Atom {
    fn from(tag: HashTag) -> Self {
        Atom::HashTag(tag)
    }
}

pub fn parse_atom<'a>() -> impl Parser<'a, &'a str, Atom, extra::Err<Rich<'a, char>>> {
    // atoms form the base of the expression tree
    choice((
        // keyword and constant atoms go first
        parse_bool().map(Atom::Bool),
        parse_ulid().map(Atom::Ulid),
        parse_duration().map(Atom::Duration),
        // dates ahead of numbers, otherwise they can look like minus expressions
        parse_datetime().map(Atom::DateTime),
        parse_number().map(Atom::Number),
        parse_hashtag().map(Atom::HashTag),
        parse_string().map(Atom::String),
        parse_regex().map(|r| Atom::Regex(r.into())),
    ))
}
