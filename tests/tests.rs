use pseudocode::prelude::*;
use std::fs;

#[test]
fn test_primitives() {
    let code = fs::read_to_string("tests/data/test_primitives.pseudo").unwrap();

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

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_primitive_type_check() {
    let code = fs::read_to_string("tests/data/test_primitive_type_check.pseudo").unwrap();

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

    assert!(interpreter.interpret(code).is_err());
}

#[test]
fn test_array_primitives() {
    let code = fs::read_to_string("tests/data/test_array_primitive.pseudo").unwrap();

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

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_array_out_of_range_index() {
    let code = fs::read_to_string("tests/data/test_out_of_range_index.pseudo").unwrap();

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

    assert!(interpreter.interpret(code).is_err());
}

#[test]
fn test_composite_primitives() {
    let code = fs::read_to_string("tests/data/test_composite_primitive.pseudo").unwrap();

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

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_if_else() {
    let code = fs::read_to_string("tests/data/test_if_else.pseudo").unwrap();
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

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_non_key_access_composite() {
    let code = fs::read_to_string("tests/data/test_non_key_access_composite.pseudo").unwrap();
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

    assert!(interpreter.interpret(code).is_err());
}

#[test]
fn test_procedure_primitive() {
    let code = fs::read_to_string("tests/data/test_procedure_primitive.pseudo").unwrap();
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

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_procedure_composite() {
    let code = fs::read_to_string("tests/data/test_procedure_composite.pseudo").unwrap();
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

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_array_composite() {
    let code = fs::read_to_string("tests/data/test_array_composite.pseudo").unwrap();
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

    assert!(interpreter.interpret(code).is_ok());
}
