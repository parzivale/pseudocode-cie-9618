use std::collections::HashMap;

use ariadne::{Color, Fmt, Label, Report, ReportKind};

mod eval;
mod lexer;
mod parser;
mod prelude;
mod utils;
use eval::*;
use lexer::*;
use parser::*;
use prelude::*;

pub fn interpret(code: String) -> Result<(), Vec<ariadne::Report>> {
    if cfg!(debug_assertions) {
        println!("--- Code INPUT ---\n{}\n", code);
    }

    let (tokens, mut errs) = lexer().parse_recovery(code.clone());

    let parse_errs = if let Some(tokens) = tokens {
        let eoi = 0..code.chars().count();
        if cfg!(debug_assertions) {
            println!("--- Token Trees ---\n{:#?}\n", tokens);
        }
        let token_stream = tts_to_stream(eoi, tokens);

        let (ast, parse_errs) = parser().parse_recovery(token_stream);

        let mut types = HashMap::from([
            ("INTEGER".to_string(), Types::Integer),
            ("REAL".to_string(), Types::Real),
            ("BOOLEAN".to_string(), Types::Boolean),
            ("STRING".to_string(), Types::String),
            ("CHAR".to_string(), Types::Char),
            ("NULL".to_string(), Types::Null),
        ]);
        if let Some(ast) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
            if cfg!(debug_assertions) {
                println!("--- Abstract Syntax Tree ---\n{:#?}\n", ast);
            }
            println!("--- OUTPUT ---");
            match eval(&ast, &mut HashMap::new(), &mut HashMap::new(), &mut types) {
                Ok(_) => {}
                Err(e) => errs.push(Simple::custom(e.span, e.msg)),
            }
        }
        parse_errs
    } else {
        Vec::new()
    };

    if !errs.is_empty() || !parse_errs.is_empty() {
        Err(errs
            .into_iter()
            .map(|e| e.map(|c| c.to_string()))
            .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
            .map(|e| {
                let report = Report::build(ReportKind::Error, (), e.span().start);

                let report = match e.reason() {
                    chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_label(
                            Label::new(span.clone())
                                .with_message(format!(
                                    "Unclosed delimiter {}",
                                    delimiter.fg(Color::Yellow)
                                ))
                                .with_color(Color::Yellow),
                        )
                        .with_label(
                            Label::new(e.span())
                                .with_message(format!(
                                    "Must be closed before this {}",
                                    e.found()
                                        .unwrap_or(&"end of file".to_string())
                                        .fg(Color::Red)
                                ))
                                .with_color(Color::Red),
                        ),
                    chumsky::error::SimpleReason::Unexpected => report
                        .with_message(format!(
                            "{}, expected {}",
                            if e.found().is_some() {
                                "Unexpected token in input"
                            } else {
                                "Unexpected end of input"
                            },
                            if e.expected().len() == 0 {
                                "something else".to_string()
                            } else {
                                e.expected()
                                    .map(|expected| match expected {
                                        Some(expected) => expected.to_string(),
                                        None => "end of input".to_string(),
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            }
                        ))
                        .with_label(
                            Label::new(e.span())
                                .with_message(format!(
                                    "Unexpected token {}",
                                    e.found()
                                        .unwrap_or(&"end of file".to_string())
                                        .fg(Color::Red)
                                ))
                                .with_color(Color::Red),
                        ),
                    chumsky::error::SimpleReason::Custom(msg) => {
                        report.with_message(msg).with_label(
                            Label::new(e.span())
                                .with_message(format!("{}", msg.fg(Color::Red)))
                                .with_color(Color::Red),
                        )
                    }
                };

                report.finish()
            })
            .collect())
    } else {
        Ok(())
    }
}
