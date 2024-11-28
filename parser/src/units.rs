use super::identifiers::*;
use super::numbers::*;
use chumsky::prelude::*;
use rust_decimal::prelude::*;
use std::fmt;

/// A unit of measurement.
///
/// These can be any identifier you like.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
pub struct Unit(Identifier);

impl Unit {
    /// Creates a new unit from a string.
    pub fn of<S: Into<Identifier>>(s: S) -> Self {
        Unit(s.into())
    }
}

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
        .then(parse_non_numeric_identifier())
        .map(|(number, identifier)| Measure {
            number,
            unit: identifier.into(),
        })
}

/// Time units for durations.
enum TimeUnit {
    Nanosecond,
    Microsecond,
    Millisecond,
    Second,
    Minute,
    Hour,
    Day,
}

impl TimeUnit {
    /// Returns the number of nanoseconds in a single unit of time.
    fn nanos(&self) -> i64 {
        match self {
            TimeUnit::Nanosecond => 1,
            TimeUnit::Microsecond => 1_000,
            TimeUnit::Millisecond => 1_000_000,
            TimeUnit::Second => 1_000_000_000,
            TimeUnit::Minute => 60 * 1_000_000_000,
            TimeUnit::Hour => 60 * 60 * 1_000_000_000,
            TimeUnit::Day => 24 * 60 * 60 * 1_000_000_000,
        }
    }
}

/// Parses a `chrono::Duration` from a string.
///
/// Example:
///
/// ```
/// use chumsky::Parser;
/// use chrono::{Duration, Utc};
/// use cel_parser::units::parse_duration;
///
/// let duration_str = "10s";
/// let parsed_duration = parse_duration().parse(duration_str).unwrap();
/// assert_eq!(parsed_duration, Duration::seconds(10));
///
/// let now = Utc::now();
/// let future = now + parsed_duration;
/// let difference = future - now;
/// assert_eq!(difference, Duration::seconds(10));
///
/// // and compund durations
/// let compound = "1m10s";
/// let parsed_compound = parse_duration().parse(compound).unwrap();
/// assert_eq!(parsed_compound, Duration::minutes(1) +  Duration::seconds(10));
/// ```
pub fn parse_duration<'a>() -> impl Parser<'a, &'a str, chrono::Duration, extra::Err<Rich<'a, char>>>
{
    // this is a bit funky, chaining parsers together
    parse_measure()
        .repeated()
        .at_least(1)
        .collect()
        .map(|measures: Vec<Measure>| {
            measures
                .into_iter()
                .flat_map(|measure| {
                    let time = match measure.unit.0.to_string().as_str() {
                        "ns" => Ok(TimeUnit::Nanosecond),
                        "us" => Ok(TimeUnit::Microsecond),
                        "ms" => Ok(TimeUnit::Millisecond),
                        "s" => Ok(TimeUnit::Second),
                        "m" => Ok(TimeUnit::Minute),
                        "h" => Ok(TimeUnit::Hour),
                        "d" => Ok(TimeUnit::Day),
                        _ => Err("unknown unit"),
                    };
                    match time {
                        Ok(unit) => Ok(chrono::Duration::nanoseconds(
                            (measure.number * Decimal::from(unit.nanos()))
                                .to_i64()
                                .unwrap(),
                        )),
                        Err(_) => Err("unknown unit"),
                    }
                })
                .fold(chrono::Duration::nanoseconds(0), |acc, x| acc + x)
        })
        .map(|unit| unit)
}
