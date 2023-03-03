use std::{
    collections::HashMap,
    sync::{mpsc::{channel, Receiver, Sender}, Arc, Mutex},
    thread, io,
};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

mod eval;
mod lexer;
mod parser;
mod prelude;
mod utils;
use eval::*;
use lexer::*;
use parser::*;
use prelude::*;

pub enum Actions {
    Output(String),
    Input,
}

pub struct Interpreter {
    source: String,
    input: Box<dyn Fn(&mut Self) -> io::Result<()>>,
    output: Box<dyn Fn(String)>,
    reciver: Receiver<Actions>,
    sender: Sender<Actions>,
    input_buffer: Arc<Mutex<String>>
}

impl Default for Interpreter {
    fn default() -> Self {
        let (sender, reciver) = channel();
        Self {
            source: "".to_string(),
            input: Box::new(|interpreter| {
                let stdin = io::stdin();
                let mut buf = String::new();
                stdin.read_line(&mut buf)?;
                *interpreter.input_buffer.lock().unwrap() = buf;
                Ok(())
            }),
            output: Box::new(|string| {
                println!("{}", string);

            }),
            reciver,
            sender,
            input_buffer: Arc::new(Mutex::new("".to_string())),
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_output(&mut self, func: Box<dyn Fn(String)>) -> &mut Self {
        self.output = func;
        self
    }

    pub fn set_input(&mut self, func: Box<dyn Fn(&mut Self)->io::Result<()>>) -> &mut Self {
        self.input = func;
        self
    }



    pub fn interpret(&mut self) -> Result<(), Vec<ariadne::Report>> {
        if cfg!(debug_assertions) {
            println!("--- Code INPUT ---\n{}\n", self.source);
        }

        let (tokens, mut errs) = lexer().parse_recovery(self.source.clone());

        let parse_errs = if let Some(tokens) = tokens {
            let eoi = 0..self.source.chars().count();
            if cfg!(debug_assertions) {
                println!("--- Token Trees ---\n{:#?}\n", tokens);
            }
            let token_stream = tts_to_stream(eoi, tokens);

            let (ast, parse_errs) = parser().parse_recovery(token_stream);

            let types = HashMap::from([
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

                let mut ctx = Ctx::new();
                ctx.types = types;
                ctx.channel = self.sender.clone();

                let interpreter = thread::spawn(move || eval(&ast, &mut ctx));
                for i in &self.reciver {
                    match i {
                        Actions::Output(s) => (self.output)(s),
                        Actions::Input => if let Err(err) = (self.input)(self) {
                            errs.push(Simple::custom(0..0, err.to_string()))
                        },
                        _ => {}
                    }
                }

                let response = interpreter.join().unwrap();
                match response {
                    Ok(_) => {},
                    Err(err) => errs.push(Simple::custom(err.span, err.msg))
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
}
