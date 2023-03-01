use pseudocode::interpret;

use std::{env, fs};

use ariadne::Source;

fn main() {
    let code = "OUTPUT \"HELLO World\"".to_string();

    match pseudocode::interpret(code.clone()) {
        Err(e) => {
            for i in e {
                i.eprint(Source::from(&code));
            }
        }
        _ => {}
    };
}
