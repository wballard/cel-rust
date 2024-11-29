use std::io::Write;

use crate::ast::Token;
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub struct ParseErrors {
    pub messages: Vec<ParseErrorMessage>,
}

impl ParseErrors {
    pub fn from_text(e: Vec<Rich<char>>) -> Self {
        Self {
            messages: e.into_iter().map(ParseErrorMessage::from_text).collect(),
        }
    }
    pub fn from_parser(e: Vec<Rich<Token>>) -> Self {
        Self {
            messages: e.into_iter().map(ParseErrorMessage::from_token).collect(),
        }
    }
}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for msg in &self.messages {
            writeln!(f, "{}", msg)?;
        }
        Ok(())
    }
}

#[derive(Error, Debug)]
#[error("{span}: {msg}")]
pub struct ParseErrorMessage {
    pub msg: String,
    pub span: SimpleSpan<usize>,
}

impl ParseErrorMessage {
    pub fn from_text(e: Rich<char>) -> Self {
        let report = Report::build(ReportKind::Error, e.span().into_range())
            .with_message(e.to_string())
            .with_label(
                Label::new(e.span().into_range())
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish();
        let mut buf = Vec::new();
        match report.write(Source::from(""), &mut buf) {
            Ok(s) => s,
            Err(_) => {
                buf.write_all("Error writing report".as_bytes()).unwrap();
            }
        }

        Self {
            msg: String::from_utf8(buf).unwrap(),
            span: *e.span(), // copy the span
        }
    }
    pub fn from_token(e: Rich<Token>) -> Self {
        Self {
            msg: format!("{}", e.reason()),
            span: *e.span(), // copy the span
        }
    }
}
