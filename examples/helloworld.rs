use pseudocode_cie_9618::prelude::*;

fn main() {
    let code = "OUTPUT \"HELLO World\"".to_string();
    let mut interpreter = Interpreter::default();

    if let Err(e) = interpreter.interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code)).unwrap();
        }
    }
}
