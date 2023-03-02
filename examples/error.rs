use ariadne::Source;

fn main() {
    let code = "DECLARE i:INTEGER\ni<-\"I am an error!\"".to_string();

    if let Err(e) = pseudocode::interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code));
        }
    }
}
