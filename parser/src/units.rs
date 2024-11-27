use super::identifiers::*;
use super::numbers::*;
use chumsky::prelude::*;
use rust_decimal::Decimal;
use std::fmt;

/// A unit of measurement.
///
/// These can be any identifier you like.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
pub struct Unit(Identifier);

impl From<Identifier> for Unit {
    fn from(identifier: Identifier) -> Self {
        Unit(identifier)
    }
}

impl From<&str> for Unit {
    fn from(identifier: &str) -> Self {
        Unit(identifier.into())
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A single measure combines a number and a unit.
///
/// The idea here is to not just 'have numbers' - but have them with units
/// so that it is clear what the number represents.

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
pub struct Measure {
    pub number: Decimal,
    pub unit: Unit,
}

impl fmt::Display for Measure {
    /// Formats the `Measure` as a string. This is parseable back into a `Measure`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.number, self.unit)
    }
}

/// Parses a measure from a string.
///
/// # Examples
///
/// ```
/// use chumsky::Parser;
/// use rust_decimal::Decimal;
/// use cel_parser::units::{parse_measure, Measure, Unit};
/// use cel_parser::identifiers::Identifier;
///
/// let measure_str = "42crabs";
/// let parsed_measure = parse_measure().parse(measure_str).unwrap();
/// assert_eq!(parsed_measure, Measure {
///     number: Decimal::new(42, 0),
///     unit: "crabs".into(),
/// });
/// assert_eq!(measure_str, parsed_measure.to_string());
/// ```
pub fn parse_measure<'a>() -> impl Parser<'a, &'a str, Measure, extra::Err<Rich<'a, char>>> {
    parse_number()
        .then(parse_identifier())
        .map(|(number, identifier)| Measure {
            number,
            unit: identifier.into(),
        })
}
