use std::{
    collections::HashMap,
    error::Error,
    fs::{self, File},
    io::{self, Write},
    path::{self, Path},
    rc::Rc,
    sync::{
        mpsc::{channel, Receiver, RecvTimeoutError, Sender},
        Arc, Mutex,
    },
    thread,
    time::Duration,
};

use ariadne::{Color, Fmt, Label, Report, ReportKind};

mod bin_ops;
mod builtins;
mod eval;
mod lexer;
mod parser;
pub mod prelude;
mod utils;
use eval::*;
use lexer::*;
use parser::*;
pub use prelude::*;

#[derive(Clone, Debug)]
pub enum Actions {
    Output(String),
    Input,
    Read(String),
    Write(String, String),
    Append(String, String),
}

#[derive(Clone, Debug)]
pub enum WriteMode {
    Write,
    Append,
}

#[derive(Clone)]
pub struct Interpreter<I, O, R, W> {
    pub input: I,
    pub output: O,
    pub reader: R,
    pub writer: W,
    pub reciver: Rc<Receiver<Actions>>,
    pub sender: Sender<Actions>,
    pub input_buffer: Arc<Mutex<String>>,
    pub file_buffer: Arc<Mutex<String>>,
}

impl<I, O, R, W> Interpreter<I, O, R, W>
where
    I: Fn(&mut String) -> Result<(), io::Error> + Clone,
    O: Fn(String) + Clone,
    R: Fn(String, &mut String) -> Result<(), io::Error> + Clone,
    W: Fn(String, String) -> Result<(), io::Error> + Clone,
{
    pub fn new(input: I, output: O, reader: R, writer: W) -> Self {
        let (sender, reciver) = channel();
        Self {
            input,
            output,
            reader,
            writer,
            reciver: Rc::new(reciver),
            sender,
            input_buffer: Arc::new(Mutex::new("".to_string())),
            file_buffer: Arc::new(Mutex::new("".to_string())),
        }
    }

    pub fn interpret(&mut self, code: String) -> Result<(), Vec<ariadne::Report>> {
        // chumksy doesnt like newlines in a nested statement, this is to handle that
        let code = code
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| format!("{}\n", line))
            .collect::<String>();

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

            let right = Builtin {
                args: vec!["STRING".to_string(), "INTEGER".to_string()],
                returns: "STRING".to_string(),
            };

            let length = Builtin {
                args: vec!["STRING".to_string()],
                returns: "INTEGER".to_string(),
            };

            let mid = Builtin {
                args: vec![
                    "STRING".to_string(),
                    "INTEGER".to_string(),
                    "INTEGER".to_string(),
                ],
                returns: "STRING".to_string(),
            };

            let lcase = Builtin {
                args: vec!["CHAR".to_string()],
                returns: "CHAR".to_string(),
            };

            let ucase = Builtin {
                args: vec!["CHAR".to_string()],
                returns: "CHAR".to_string(),
            };

            let real_to_int = Builtin {
                args: vec!["REAL".to_string()],
                returns: "INTEGER".to_string(),
            };

            let int_to_real = Builtin {
                args: vec!["INTEGER".to_string()],
                returns: "REAL".to_string(),
            };

            let asc = Builtin {
                args: vec!["CHAR".to_string()],
                returns: "INTEGER".to_string(),
            };

            let mod_ = Builtin {
                args: vec!["INTEGER".to_string(), "INTEGER".to_string()],
                returns: "INTEGER".to_string(),
            };

            let integer_to_string = Builtin {
                args: vec!["INTEGER".to_string()],
                returns: "STRING".to_string(),
            };

            let real_to_string = Builtin {
                args: vec!["REAL".to_string()],
                returns: "STRING".to_string(),
            };

            let char_to_string = Builtin {
                args: vec!["CHAR".to_string()],
                returns: "STRING".to_string(),
            };

            let string_to_char = Builtin {
                args: vec!["STRING".to_string()],
                returns: "CHAR".to_string(),
            };

            let string_to_real = Builtin {
                args: vec!["STRING".to_string()],
                returns: "REAL".to_string(),
            };
            let string_to_integer = Builtin {
                args: vec!["STRING".to_string()],
                returns: "INTEGER".to_string(),
            };

            let char_to_integer = Builtin {
                args: vec!["CHAR".to_string()],
                returns: "INTEGER".to_string(),
            };

            let char_to_real = Builtin {
                args: vec!["CHAR".to_string()],
                returns: "REAL".to_string(),
            };

            let types = HashMap::from([
                ("INTEGER".to_string(), Types::Integer),
                ("REAL".to_string(), Types::Real),
                ("BOOLEAN".to_string(), Types::Boolean),
                ("STRING".to_string(), Types::String),
                ("CHAR".to_string(), Types::Char),
                ("NULL".to_string(), Types::Null),
                ("RIGHT".to_string(), Types::Builtin(right)),
                ("LENGTH".to_string(), Types::Builtin(length)),
                ("MID".to_string(), Types::Builtin(mid)),
                ("LCASE".to_string(), Types::Builtin(lcase)),
                ("UCASE".to_string(), Types::Builtin(ucase)),
                ("REAL_TO_INTEGER".to_string(), Types::Builtin(real_to_int)),
                ("INTEGER_TO_REAL".to_string(), Types::Builtin(int_to_real)),
                ("ASC".to_string(), Types::Builtin(asc)),
                ("MOD".to_string(), Types::Builtin(mod_)),
                (
                    "INTEGER_TO_STRING".to_string(),
                    Types::Builtin(integer_to_string),
                ),
                ("REAL_TO_STRING".to_string(), Types::Builtin(real_to_string)),
                ("CHAR_TO_STRING".to_string(), Types::Builtin(char_to_string)),
                ("STRING_TO_CHAR".to_string(), Types::Builtin(string_to_char)),
                ("STRING_TO_REAL".to_string(), Types::Builtin(string_to_real)),
                (
                    "STRING_TO_INTEGER".to_string(),
                    Types::Builtin(string_to_integer),
                ),
                (
                    "CHAR_TO_INTEGER".to_string(),
                    Types::Builtin(char_to_integer),
                ),
                ("CHAR_TO_REAL".to_string(), Types::Builtin(char_to_real)),
            ]);
            if let Some(ast) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
                if cfg!(debug_assertions) {
                    println!("--- Abstract Syntax Tree ---\n{:#?}\n", ast);
                    println!("--- OUTPUT ---");
                }

                let mut ctx = Ctx::new();
                ctx.types = types;
                ctx.input = Arc::clone(&self.input_buffer);
                ctx.channel = self.sender.clone();
                let interpreter = thread::spawn(move || eval(&ast, &mut ctx));

                'top: loop {
                    match self.reciver.recv_timeout(Duration::from_millis(10)) {
                        Ok(Actions::Output(s)) => {
                            (self.output.clone())(s);
                        }
                        Ok(Actions::Input) => {
                            if let Err(err) =
                                (self.input.clone())(&mut self.input_buffer.lock().unwrap())
                            {
                                errs.push(Simple::custom(0..0, err.to_string()));
                                interpreter.thread().unpark();
                            } else {
                                interpreter.thread().unpark();
                            }
                        }
                        Ok(Actions::Read(file)) => {
                            if let Err(err) =
                                (self.reader.clone())(file, &mut self.file_buffer.lock().unwrap())
                            {
                                errs.push(Simple::custom(0..0, err.to_string()));
                                interpreter.thread().unpark();
                            } else {
                                interpreter.thread().unpark();
                            }
                        }
                        Ok(Actions::Write(file, data)) => {
                            if let Err(err) = (self.writer.clone())(file, data) {
                                errs.push(Simple::custom(0..0, err.to_string()));
                                interpreter.thread().unpark();
                            } else {
                                interpreter.thread().unpark();
                            }
                        }
                        Err(RecvTimeoutError::Timeout) => {
                            if interpreter.is_finished() {
                                break 'top;
                            }
                        }
                        e => {
                            println!("{:?}", e);
                            break 'top;
                        }
                    }
                }

                let response = interpreter.join();
                match response {
                    Ok(Ok(_)) => {}
                    Ok(Err(err)) => errs.push(Simple::custom(err.span, err.msg)),
                    Err(_) => errs.push(Simple::custom(0..0, "Thread panicked".to_string())),
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

impl Default
    for Interpreter<
        fn(&mut String) -> Result<(), io::Error>,
        fn(String),
        fn(String, &mut String) -> Result<(), io::Error>,
        fn(String, String) -> Result<(), io::Error>,
    >
{
    fn default() -> Self {
        let input = |s: &mut String| -> Result<(), io::Error> {
            let stdin = io::stdin();
            let mut buf = String::new();
            stdin.read_line(&mut buf)?;
            *s = buf;
            Ok(())
        };

        let output = |s: String| {
            println!("{}", s);
        };

        let reader = |f: String, s: &mut String| -> Result<(), io::Error> {
            let path = Path::new(&f);
            let file = fs::File::open(path)?;
            Ok(())
        };

        let writer = |f: String, s: String| Ok(());

        Interpreter::new(input, output, reader, writer)
    }
}
