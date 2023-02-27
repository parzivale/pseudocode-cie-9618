use std::{env, fs};

use ariadne::Source;

fn main() {
    let code = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    match pseudocode::interpret(code.clone()) {
        Err(e) => {
            for i in e {
                i.eprint(Source::from(&code));
            }
        }
        _ => {}
    };
}
