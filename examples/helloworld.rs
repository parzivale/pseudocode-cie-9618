use pseudocode::prelude::*;

fn main() {
    let code = "OUTPUT \"HELLO World\"".to_string();

    let input = |s: &mut String| -> Result<(), io::Error> {
        let stdin = io::stdin();
        let mut buf = String::new();
        stdin.read_line(&mut buf)?;
        *s = buf;
        Ok(())
    };

    let output = |s: String| {
        println!("{}", s);
    };

    let mut interpreter = Interpreter::new(input, output);

    if let Err(e) = interpreter.interpret(code.clone()) {
        for i in e {
            i.eprint(Source::from(&code));
        }
    }
}
