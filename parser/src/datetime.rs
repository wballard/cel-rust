use chrono::prelude::*;
use chumsky::prelude::*;

/// Parse a timestamp in various formats including naive datetime, naive datetime with milliseconds, and RFC3339 format.
///
/// # Examples
///
/// ```
/// use chrono::Utc;
/// use chrono::DateTime;
/// use chumsky::prelude::*;
/// use cel_parser::datetime::parse_datetime;
///
/// let input = "2023-05-28T00:00:42Z";
/// let expected = DateTime::parse_from_rfc3339("2023-05-28T00:00:42Z").unwrap().with_timezone(&Utc);
/// let result: DateTime<Utc> = parse_datetime().parse(input).unwrap();
/// assert_eq!(result, expected);
///
/// let input = "2023-05-28T00:00:42.123Z";
/// let expected = DateTime::parse_from_rfc3339("2023-05-28T00:00:42.123Z").unwrap().with_timezone(&Utc);
/// let result: DateTime<Utc> = parse_datetime().parse(input).unwrap();
/// assert_eq!(result, expected);
///
/// let input = "2023-05-28T00:00:42+00:00";
/// let expected = DateTime::parse_from_rfc3339("2023-05-28T00:00:42+00:00").unwrap().with_timezone(&Utc);
/// let result: DateTime<Utc> = parse_datetime().parse(input).unwrap();
/// assert_eq!(result, expected);
///
/// let input = "2023-05-28T00:00:42.123+00:00";
/// let expected = DateTime::parse_from_rfc3339("2023-05-28T00:00:42.123+00:00").unwrap().with_timezone(&Utc);
/// let result: DateTime<Utc> = parse_datetime().parse(input).unwrap();
/// assert_eq!(result, expected);
//
/// // and a 'bare date' with a T suffix to disambiguate from arithmetic
/// let input = "2023-05-28T";
/// let expected = DateTime::parse_from_rfc3339("2023-05-28T00:00:00Z").unwrap().with_timezone(&Utc);
/// let result: DateTime<Utc> = parse_datetime().parse(input).unwrap();
/// assert_eq!(result, expected);
/// ```
pub fn parse_datetime<'a>(
) -> impl Parser<'a, &'a str, chrono::DateTime<chrono::Utc>, extra::Err<Rich<'a, char>>> {
    choice((
        parse_naive_datetime(),
        parse_naive_datetime_millis(),
        parse_rfc3399(),
        parse_date(),
    ))
}

fn parse_date<'a>(
) -> impl Parser<'a, &'a str, chrono::DateTime<chrono::Utc>, extra::Err<Rich<'a, char>>> {
    let pattern = r"\d{4}-\d{2}-\d{2}T";
    regex(pattern).map(|s| {
        chrono::DateTime::<chrono::Utc>::from_naive_utc_and_offset(
            NaiveDate::parse_from_str(s, "%Y-%m-%dT")
                .unwrap()
                .and_time(NaiveTime::default()),
            chrono::Utc,
        )
    })
}

fn parse_naive_datetime<'a>(
) -> impl Parser<'a, &'a str, chrono::DateTime<chrono::Utc>, extra::Err<Rich<'a, char>>> {
    let pattern = r"d{4}-\d{2}-\d{2}T[0-9]{2}:[0-9]{2}(.[0-9]{1,8})?";
    regex(pattern).map(|s| {
        chrono::DateTime::<chrono::Utc>::from_naive_utc_and_offset(
            chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S").unwrap(),
            chrono::Utc,
        )
    })
}

fn parse_naive_datetime_millis<'a>(
) -> impl Parser<'a, &'a str, chrono::DateTime<chrono::Utc>, extra::Err<Rich<'a, char>>> {
    let pattern = r"d{4}-\d{2}-\d{2}T[0-9]{2}:[0-9]{2}(.[0-9]{1,8})?";
    regex(pattern).map(|s| {
        chrono::DateTime::<chrono::Utc>::from_naive_utc_and_offset(
            chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S%.f").unwrap(),
            chrono::Utc,
        )
    })
}

fn parse_rfc3399<'a>(
) -> impl Parser<'a, &'a str, chrono::DateTime<chrono::Utc>, extra::Err<Rich<'a, char>>> {
    let pattern = r"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(.[0-9]{1,8})?(([+-][0-9]{2}:[0-9]{2})|Z)";
    regex(pattern).map(|s| chrono::DateTime::parse_from_rfc3339(s).unwrap().into())
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("2023-05-28T00:00:42Z", chrono::DateTime::parse_from_rfc3339("2023-05-28T00:00:42Z").unwrap().with_timezone(&chrono::Utc))]
    #[case("2023-05-28T00:00:42.123Z", chrono::DateTime::parse_from_rfc3339("2023-05-28T00:00:42.123Z").unwrap().with_timezone(&chrono::Utc))]
    #[case("2023-05-28T00:00:42+00:00", chrono::DateTime::parse_from_rfc3339("2023-05-28T00:00:42+00:00").unwrap().with_timezone(&chrono::Utc))]
    #[case("2023-05-28T00:00:42.123+00:00", chrono::DateTime::parse_from_rfc3339("2023-05-28T00:00:42.123+00:00").unwrap().with_timezone(&chrono::Utc))]
    #[case("2023-05-28T", chrono::DateTime::parse_from_rfc3339("2023-05-28T00:00:00Z").unwrap().with_timezone(&chrono::Utc))]
    fn test_parse_datetime(#[case] input: &str, #[case] expected: chrono::DateTime<chrono::Utc>) {
        let result: chrono::DateTime<chrono::Utc> = parse_datetime().parse(input).unwrap();
        assert_eq!(result, expected);
    }
}
