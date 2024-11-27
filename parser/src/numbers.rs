use chumsky::prelude::*;
use rust_decimal::prelude::*;

fn parse_decimal<'a>() -> impl Parser<'a, &'a str, Decimal, extra::Err<Rich<'a, char>>> {
    let digits = text::digits(10).to_slice();
    let frac = just('.').then(digits);
    just('-')
        .or_not()
        .then(digits)
        .then(frac.or_not())
        .to_slice()
        .map(|s: &str| Decimal::from_str(s).unwrap())
}

fn parse_scientific<'a>() -> impl Parser<'a, &'a str, Decimal, extra::Err<Rich<'a, char>>> {
    let digits = text::digits(10).to_slice();
    let frac = just('.').then(digits);
    let exp = choice((just('e'), just('E')))
        .then(one_of("+-").or_not())
        .then(digits);
    just('-')
        .or_not()
        .then(digits)
        .then(frac.or_not())
        .then(exp)
        .to_slice()
        .map(|s: &str| Decimal::from_scientific(s).unwrap())
}

fn parse_hexadecimal<'a>() -> impl Parser<'a, &'a str, Decimal, extra::Err<Rich<'a, char>>> {
    let digits = text::digits(16).to_slice();
    just("x").or(just("X")).ignore_then(
        just('-')
            .or_not()
            .then(digits)
            .to_slice()
            .map(|s: &str| Decimal::from_str_radix(s, 16).unwrap()),
    )
}

/// Parses a number from a string, supporting decimal, scientific, and hexadecimal formats.
///
/// # Examples
///
/// ```
/// use cel_parser::parse_number;
/// use chumsky::prelude::*;
/// use rust_decimal::prelude::*;
///
/// // Parsing a decimal number
/// let result: Decimal = parse_number().parse("123.456").unwrap();
/// assert_eq!(result, Decimal::new(123456, 3));
///
/// // Parsing a scientific number
/// let result: Decimal = parse_number().parse("1.23e3").unwrap();
/// assert_eq!(result, Decimal::new(1230, 0));
///
/// // Parsing a hexadecimal number
/// let result: Decimal = parse_number().parse("x7B").unwrap();
/// assert_eq!(result, Decimal::new(123, 0));
/// ```
pub fn parse_number<'a>() -> impl Parser<'a, &'a str, Decimal, extra::Err<Rich<'a, char>>> {
    choice((parse_hexadecimal(), parse_scientific(), parse_decimal()))
}

// unit tests here
#[cfg(test)]
mod test {
    use super::*;
    use rstest::rstest;
    use rust_decimal_macros::dec;

    #[rstest]
    #[case("123", dec!(123))]
    #[case("0", dec!(0))]
    #[case("-123", dec!(-123))]
    #[case("123.456", dec!(123.456))]
    #[case("0.0", dec!(0.0))]
    #[case("-123.456", dec!(-123.456))]
    fn test_parse_decimal_cases(#[case] input: &str, #[case] expected: Decimal) {
        assert_eq!(parse_decimal().parse(input).unwrap(), expected);
    }

    #[rstest]
    #[case("123e3", dec!(123e3))]
    #[case("123E3", dec!(123e3))]
    #[case("123e+3", dec!(123e3))]
    #[case("123e-3", dec!(123e-3))]
    #[case("123.456e3", dec!(123.456e3))]
    #[case("123.456E3", dec!(123.456e3))]
    #[case("123.456e+3", dec!(123.456e3))]
    #[case("123.456e-3", dec!(123.456e-3))]
    fn test_parse_scientific_cases(#[case] input: &str, #[case] expected: Decimal) {
        assert_eq!(parse_scientific().parse(input).unwrap(), expected);
    }

    #[rstest]
    #[case("x7B", dec!(123))]
    #[case("x0", dec!(0))]
    #[case("X0", dec!(0))]
    #[case("x-7B", dec!(-123))]
    #[case("X-7B", dec!(-123))]
    fn test_parse_hexadecimal_cases(#[case] input: &str, #[case] expected: Decimal) {
        assert_eq!(parse_hexadecimal().parse(input).unwrap(), expected);
    }

    #[rstest]
    #[case("123", dec!(123))]
    #[case("0", dec!(0))]
    #[case("-123", dec!(-123))]
    #[case("123.456", dec!(123.456))]
    #[case("0.0", dec!(0.0))]
    #[case("-123.456", dec!(-123.456))]
    #[case("x7B", dec!(123))]
    #[case("x0", dec!(0))]
    #[case("X0", dec!(0))]
    #[case("x-7B", dec!(-123))]
    #[case("X-7B", dec!(-123))]
    #[case("123e3", dec!(123e3))]
    #[case("123E3", dec!(123e3))]
    #[case("123e+3", dec!(123e3))]
    #[case("123e-3", dec!(123e-3))]
    #[case("123.456e3", dec!(123.456e3))]
    #[case("123.456E3", dec!(123.456e3))]
    #[case("123.456e+3", dec!(123.456e3))]
    #[case("123.456e-3", dec!(123.456e-3))]
    fn test_parse_number_cases(#[case] input: &str, #[case] expected: Decimal) {
        assert_eq!(parse_number().parse(input).unwrap(), expected);
    }
}
