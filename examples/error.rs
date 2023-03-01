use pseudocode::interpret;

use std::{env, fs};

use ariadne::Source;

fn main() {
    let code = "DECLARE i:INTEGER\ni<-\"I am an error!\"".to_string();

    match pseudocode::interpret(code.clone()) {
        Err(e) => {
            for i in e {
                i.eprint(Source::from(&code));
            }
        }
        _ => {}
    };
}
