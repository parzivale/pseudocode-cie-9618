use std::fs;

use pseudocode::prelude::*;

fn main() {
    let code = fs::read_to_string("tests/data/factorial.pseudo").unwrap();
    let mut interpreter = Interpreter::default();

    if let Err(e) = interpreter.interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code)).unwrap();
        }
    }
}
