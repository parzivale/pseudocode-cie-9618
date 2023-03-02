use ariadne::Source;

fn main() {
    let code = "OUTPUT \"HELLO World\"".to_string();

    if let Err(e) = pseudocode::interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code));
        }
    }
}
