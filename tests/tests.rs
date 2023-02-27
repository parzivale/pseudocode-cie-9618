use std::fs;

#[test]
fn test_strings_non_error() {
    let code =
        fs::read_to_string("tests\\data\\primitive tests\\string tests\\StringTestNonError.pseudo")
            .unwrap();

    assert!(pseudocode::interpret(code).is_ok());
}

#[test]
fn test_strings_error() {
    let code =
        fs::read_to_string("tests\\data\\primitive tests\\string tests\\StringTestError.pseudo")
            .unwrap();

    assert!(pseudocode::interpret(code).is_err());
}

#[test]
fn test_chars_non_error() {
    let code =
        fs::read_to_string("tests\\data\\primitive tests\\char tests\\CharTestNonError.pseudo")
            .unwrap();

    assert!(pseudocode::interpret(code).is_ok());
}

#[test]
fn test_chars_error() {
    let code = fs::read_to_string("tests\\data\\primitive tests\\char tests\\CharTestError.pseudo")
        .unwrap();

    assert!(pseudocode::interpret(code).is_err());
}
