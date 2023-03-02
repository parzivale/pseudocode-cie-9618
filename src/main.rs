use std::{env, error::Error, fs};

use ariadne::Source;

fn main() -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    if let Err(e) = pseudocode::interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code))?;
        }
    }

    Ok(())
}
