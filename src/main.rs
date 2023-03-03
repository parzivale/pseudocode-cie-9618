use std::{env, error::Error, fs, io, io::BufRead};

use ariadne::Source;
use pseudocode::Interpreter;

fn main() -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    let input = |s: &mut String| -> Result<(), io::Error> {
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        handle.read_line(s).unwrap();
        Ok(())
    };

    let output = |s: String| {
        println!("{}", s);
    };

    let mut interpreter = Interpreter::new(input, output);

    if let Err(e) = interpreter.interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code))?;
        }
    }

    Ok(())
}
