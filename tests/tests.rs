use pseudocode::prelude::*;
use std::fs;

#[test]
fn test_primitives() {
    let code = fs::read_to_string("tests/data/test_primitives.pseudo").unwrap();
    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        output_values.lock().unwrap().push(s);
    };
    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(
        *output_values.lock().unwrap(),
        vec!["1 ", "2 ", "hi there ", "a ", "true "]
    );
}

#[test]
fn test_primitive_type_check() {
    let code = fs::read_to_string("tests/data/test_primitive_type_check.pseudo").unwrap();

    let mut interpreter = Interpreter::default();

    assert!(interpreter.interpret(code).is_err());
}

#[test]
fn test_array_primitives() {
    let code = fs::read_to_string("tests/data/test_array_primitive.pseudo").unwrap();
    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        output_values.lock().unwrap().push(s);
    };
    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(
        *output_values.lock().unwrap(),
        vec!["1 ", "2 ", "hi there ", "a ", "true "]
    );
}

#[test]
fn test_array_out_of_range_index() {
    let code = fs::read_to_string("tests/data/test_out_of_range_index.pseudo").unwrap();

    let mut interpreter = Interpreter::default();

    assert!(interpreter.interpret(code).is_err());
}

#[test]
fn test_composite_primitives() {
    let code = fs::read_to_string("tests/data/test_composite_primitive.pseudo").unwrap();

    let mut interpreter = Interpreter::default();

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_if_else() {
    let code = fs::read_to_string("tests/data/test_if_else.pseudo").unwrap();
    let mut interpreter = Interpreter::default();

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_non_key_access_composite() {
    let code = fs::read_to_string("tests/data/test_non_key_access_composite.pseudo").unwrap();

    let mut interpreter = Interpreter::default();

    assert!(interpreter.interpret(code).is_err());
}

#[test]
fn test_procedure_primitive() {
    let code = fs::read_to_string("tests/data/test_procedure_primitive.pseudo").unwrap();

    let mut interpreter = Interpreter::default();

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_procedure_composite() {
    let code = fs::read_to_string("tests/data/test_procedure_composite.pseudo").unwrap();

    let mut interpreter = Interpreter::default();

    assert!(interpreter.interpret(code).is_ok());
}

#[test]
fn test_array_composite() {
    let code = fs::read_to_string("tests/data/test_array_composite.pseudo").unwrap();

    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        output_values.lock().unwrap().push(s);
    };
    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(*output_values.lock().unwrap(), vec!["1 "]);
}

#[test]
fn test_byref_byval_procedure() {
    let code = fs::read_to_string("tests/data/test_byref_byval_procedure.pseudo").unwrap();
    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        output_values.lock().unwrap().push(s);
    };

    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(*output_values.lock().unwrap(), vec!["11 ", "11 "]);
}

#[test]
fn test_function_primitives() {
    let code = fs::read_to_string("tests/data/test_function_primitives.pseudo").unwrap();

    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        output_values.lock().unwrap().push(s);
    };

    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(*output_values.lock().unwrap(), vec!["2 ", "2 "]);
}

#[test]
fn test_builtins() {
    let code = fs::read_to_string("tests/data/test_builtins.pseudo").unwrap();
    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        if !s.trim().is_empty() {
            output_values.lock().unwrap().push(s);
        }
    };
    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(
        *output_values.lock().unwrap(),
        vec![
            "ere ", "8 ", "i t ", "a ", "A ", "100 ", "99 ", "97 ", "0 ", "99 ", "100.1 ", "a ",
            "e ", "10.1 ", "15 ", "1 ", "2 "
        ]
    );
}

#[test]
fn test_while_loop() {
    let code = fs::read_to_string("tests/data/test_while_loop.pseudo").unwrap();

    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        if !s.trim().is_empty() {
            output_values.lock().unwrap().push(s);
        }
    };

    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(
        *output_values.lock().unwrap(),
        vec!["2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8 ", "9 ", "10 "]
    );
}

#[test]
fn test_for_loop() {
    let code = fs::read_to_string("tests/data/test_for_loop.pseudo").unwrap();

    let output_values = Mutex::new(Vec::new());
    let output = |s: String| {
        if !s.trim().is_empty() {
            output_values.lock().unwrap().push(s);
        }
    };

    let interpreter = Interpreter::debug_stdout(code, output);

    assert!(interpreter.is_ok());
    assert_eq!(
        *output_values.lock().unwrap(),
        vec!["1 ", "2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8 ", "9 ", "10 "]
    );
}
