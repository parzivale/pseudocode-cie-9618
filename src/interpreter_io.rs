use std::{
    fs::{self, OpenOptions},
    io::{BufReader, BufWriter, Write},
    path::Path,
    rc::Rc,
};

use crate::prelude::*;

pub type WriterRef = Rc<Mutex<BufWriter<Box<dyn Write>>>>;
pub type ReaderRef = Rc<Mutex<Box<dyn BufRead>>>;

pub trait InterpreterIO {
    fn output(&mut self, s: String) -> Result<(), io::Error> {
        println!("{}", s);
        Ok(())
    }
    fn input(&mut self, s: &mut String) -> Result<(), io::Error> {
        let stdin = io::stdin();
        let mut buf = String::new();
        stdin.read_line(&mut buf)?;
        *s = buf;
        Ok(())
    }
    fn get_reader(&mut self, f: String) -> Result<ReaderRef, io::Error> {
        let path = Path::new(&f);
        let file = fs::File::open(path)?;
        Ok(Rc::new(Mutex::new(Box::new(BufReader::new(file)))))
    }
    fn get_appender(&mut self, f: String) -> Result<WriterRef, io::Error> {
        let path = Path::new(&f);
        let file = OpenOptions::new().append(true).open(path)?;
        Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
    }
    fn get_writer(&mut self, f: String) -> Result<WriterRef, io::Error> {
        let path = Path::new(&f);
        let file = fs::File::create(path)?;
        Ok(Rc::new(Mutex::new(BufWriter::new(Box::new(file)))))
    }
}
