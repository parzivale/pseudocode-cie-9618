use std::{env, error::Error, fs};

use ariadne::Source;
use pseudocode_cie_9618::Interpreter;

fn main() -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");
    let mut interpreter = Interpreter::default();

    if let Err(e) = interpreter.interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code))?;
        }
    }

    Ok(())
}
