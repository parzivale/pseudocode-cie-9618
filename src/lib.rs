use std::{
    collections::HashMap,
    fs::{self, OpenOptions},
    io::{self, BufReader, BufWriter, Write},
    path::Path,
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
    Open(String, FileMode),
    Read(String),
    Write(String, String),
    Append(String, String),
}

#[derive(Debug)]
pub enum FileBuf<T: Write> {
    Write(BufWriter<T>),
    Read(BufReader<T>),
}

pub type WriterRef = Rc<Mutex<BufWriter<Box<dyn Write>>>>;
pub type ReaderRef = Rc<Mutex<Box<dyn BufRead>>>;

#[derive(Clone)]
pub struct InterpreterBuilder<I, O, R, A, W> {
    pub input: Option<I>,
    pub output: Option<O>,
    pub get_reader: Option<R>,
    pub get_appender: Option<A>,
    pub get_writer: Option<W>,
}

impl<I, O, R, A, W> Default for InterpreterBuilder<I, O, R, A, W> {
    fn default() -> Self {
        Self {
            input: None,
            output: None,
            get_reader: None,
            get_appender: None,
            get_writer: None,
        }
    }
}

impl<I, O, R, A, W> InterpreterBuilder<I, O, R, A, W>
where
    I: Fn(&mut String) -> Result<(), io::Error> + Clone,
    O: Fn(String) + Clone,
    R: Fn(String) -> Result<ReaderRef, io::Error> + Clone,
    A: Fn(String) -> Result<WriterRef, io::Error> + Clone,
    W: Fn(String) -> Result<WriterRef, io::Error> + Clone,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn input(&mut self, input: I) -> &mut Self {
        self.input = Some(input);
        self
    }

    pub fn output(&mut self, output: O) -> &mut Self {
        self.output = Some(output);
        self
    }

    pub fn get_appender(&mut self, get_appender: A) -> &mut Self {
        self.get_appender = Some(get_appender);
        self
    }

    pub fn get_reader(&mut self, get_reader: R) -> &mut Self {
        self.get_reader = Some(get_reader);
        self
    }

    pub fn get_writer(&mut self, get_writer: W) -> &mut Self {
        self.get_writer = Some(get_writer);
        self
    }
}

impl
    InterpreterBuilder<
        fn(&mut String) -> Result<(), io::Error>,
        fn(String),
        fn(String) -> Result<ReaderRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
    >
{
    #[allow(clippy::type_complexity)]
    pub fn build(
        self,
    ) -> Interpreter<
        fn(&mut String) -> Result<(), io::Error>,
        fn(String),
        fn(String) -> Result<ReaderRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
    > {
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

        let reader = |f: String| -> Result<ReaderRef, io::Error> {
            let path = Path::new(&f);
            let file = fs::File::open(path)?;
            Ok(Rc::new(Mutex::new(Box::new(BufReader::new(file)))))
        };

        let writer = |f: String| -> Result<WriterRef, io::Error> {
            let path = Path::new(&f);
            let file = fs::File::create(path)?;
            Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
        };

        let appender = |f: String| -> Result<WriterRef, io::Error> {
            let path = Path::new(&f);
            let file = OpenOptions::new().append(true).open(path)?;
            Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
        };

        Interpreter::new(
            self.input.unwrap_or(input),
            self.output.unwrap_or(output),
            self.get_reader.unwrap_or(reader),
            self.get_appender.unwrap_or(appender),
            self.get_writer.unwrap_or(writer),
        )
    }
}

#[derive(Clone)]
pub struct Interpreter<I, O, R, A, W> {
    pub input: I,
    pub output: O,
    pub get_reader: R,
    pub get_appender: A,
    pub get_writer: W,
    pub reciver: Rc<Receiver<Actions>>,
    pub sender: Sender<Actions>,
    pub input_buffer: Arc<Mutex<String>>,
    pub file_buffer: Arc<Mutex<String>>,
    pub open_read_files: HashMap<String, ReaderRef>,
    pub open_write_files: HashMap<String, WriterRef>,
}

impl<I, O, R, A, W> Interpreter<I, O, R, A, W>
where
    I: Fn(&mut String) -> Result<(), io::Error> + Clone,
    O: Fn(String) + Clone,
    R: Fn(String) -> Result<ReaderRef, io::Error> + Clone,
    A: Fn(String) -> Result<WriterRef, io::Error> + Clone,
    W: Fn(String) -> Result<WriterRef, io::Error> + Clone,
{
    pub fn new(input: I, output: O, get_reader: R, get_appender: A, get_writer: W) -> Self {
        let (sender, reciver) = channel();
        Self {
            input,
            output,
            get_reader,
            get_appender,
            get_writer,
            reciver: Rc::new(reciver),
            sender,
            input_buffer: Arc::new(Mutex::new("".to_string())),
            file_buffer: Arc::new(Mutex::new("".to_string())),
            open_read_files: HashMap::new(),
            open_write_files: HashMap::new(),
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
                ctx.file_read = Arc::clone(&self.file_buffer);
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
                        Ok(Actions::Open(file, mode)) => match mode {
                            FileMode::Read => match (self.get_reader.clone())(file.clone()) {
                                Ok(reader) => {
                                    self.open_read_files.insert(file, reader);
                                }
                                Err(err) => {
                                    errs.push(Simple::custom(0..0, err.to_string()));
                                }
                            },
                            FileMode::Append => match (self.get_appender.clone())(file.clone()) {
                                Ok(writer) => {
                                    self.open_write_files.insert(file, writer);
                                }
                                Err(err) => {
                                    errs.push(Simple::custom(0..0, err.to_string()));
                                }
                            },
                            FileMode::Write => match (self.get_writer.clone())(file.clone()) {
                                Ok(writer) => {
                                    self.open_write_files.insert(file, writer);
                                }
                                Err(err) => {
                                    errs.push(Simple::custom(0..0, err.to_string()));
                                }
                            },
                        },
                        Ok(Actions::Read(file)) => {
                            self.open_read_files
                                .get(&file)
                                .unwrap()
                                .lock()
                                .unwrap()
                                .read_line(&mut self.file_buffer.lock().unwrap())
                                .unwrap();
                            interpreter.thread().unpark();
                        }
                        Ok(Actions::Append(file, data)) => {
                            self.open_write_files
                                .get(&file)
                                .unwrap()
                                .lock()
                                .unwrap()
                                .write_all(data.as_bytes())
                                .unwrap();
                            interpreter.thread().unpark();
                        }
                        Ok(Actions::Write(file, data)) => {
                            self.open_write_files
                                .get(&file)
                                .unwrap()
                                .lock()
                                .unwrap()
                                .write_all(data.as_bytes())
                                .unwrap();
                            interpreter.thread().unpark();
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

impl<X>
    Interpreter<
        fn(&mut String) -> Result<(), io::Error>,
        X,
        fn(String) -> Result<ReaderRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
    >
where
    X: Fn(String) + Clone,
{
    pub fn debug_stdout(test: String, output: X) -> Result<(), Vec<ariadne::Report>> {
        let input = |s: &mut String| -> Result<(), io::Error> {
            let stdin = io::stdin();
            let mut buf = String::new();
            stdin.read_line(&mut buf)?;
            *s = buf;
            Ok(())
        };

        let reader = |f: String| -> Result<ReaderRef, io::Error> {
            let path = Path::new(&f);
            let file = fs::File::open(path)?;
            Ok(Rc::new(Mutex::new(Box::new(BufReader::new(file)))))
        };

        let writer = |f: String| -> Result<WriterRef, io::Error> {
            let path = Path::new(&f);
            let file = fs::File::create(path)?;
            Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
        };

        let appender = |f: String| -> Result<WriterRef, io::Error> {
            let path = Path::new(&f);
            let file = fs::File::open(path)?;
            Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
        };

        Self::new(input, output, reader, appender, writer).interpret(test)
    }
}

impl Default
    for Interpreter<
        fn(&mut String) -> Result<(), io::Error>,
        fn(String),
        fn(String) -> Result<ReaderRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
        fn(String) -> Result<WriterRef, io::Error>,
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

        let reader = |f: String| -> Result<ReaderRef, io::Error> {
            let path = Path::new(&f);
            let file = fs::File::open(path)?;
            Ok(Rc::new(Mutex::new(Box::new(BufReader::new(file)))))
        };

        let writer = |f: String| -> Result<WriterRef, io::Error> {
            let path = Path::new(&f);
            let file = fs::File::create(path)?;
            Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
        };

        let appender = |f: String| -> Result<WriterRef, io::Error> {
            let path = Path::new(&f);
            let file = OpenOptions::new().append(true).open(path)?;
            Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
        };

        Interpreter::new(input, output, reader, appender, writer)
    }
}
