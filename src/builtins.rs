use crate::{
    eval::Error,
    parser::{Expr, Value},
    prelude::*,
};

pub fn right(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Str(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'Right' needs a string to modify".to_string(),
            })?
            .to_string()
            .chars()
            .rev()
            .collect::<String>()
            .get(
                0..vars_func
                    .get(1)
                    .ok_or_else(|| Error {
                        span: expr.1.clone(),
                        msg: "Builtin 'Right' needs a number of characters to fetch".to_string(),
                    })?
                    .to_string()
                    .parse::<usize>()
                    .map_err(|_| Error {
                        span: expr.1.clone(),
                        msg: "Index is not an integer".to_string(),
                    })?,
            )
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'Right' needs a string to read".to_string(),
            })?
            .to_string()
            .chars()
            .rev()
            .collect::<String>(),
    ))
}

pub fn length(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Int(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'LENGTH' needs a string to modify".to_string(),
            })?
            .to_string()
            .len()
            .try_into()
            .unwrap(),
    ))
}

pub fn mid(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Str(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'MID' needs a string to modify".to_string(),
            })?
            .to_string()[vars_func
            .get(1)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'MID' needs an index to start reading from".to_string(),
            })?
            .to_string()
            .parse::<usize>()
            .map_err(|_| Error {
                span: expr.1.clone(),
                msg: "Builtin 'MID' Cannot convert starting index to integer".to_string(),
            })?
            - 1
            ..vars_func
                .get(2)
                .ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: "Builtin 'MID' needs an index to stop reading at".to_string(),
                })?
                .to_string()
                .parse::<usize>()
                .map_err(|_| Error {
                    span: expr.1.clone(),
                    msg: "Builtin 'MID' Cannot convert ending index to integer".to_string(),
                })?
                - 1
                + 1]
            .to_string(),
    ))
}

pub fn lcase(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Char(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'LCASE' needs a character to make lower case".to_string(),
            })?
            .to_string()
            .chars()
            .map(|x| x.to_ascii_lowercase())
            .collect::<String>()
            .parse::<char>()
            .unwrap(),
    ))
}

pub fn ucase(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Char(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'LCASE' needs a character to make lower case".to_string(),
            })?
            .to_string()
            .chars()
            .map(|x| x.to_ascii_uppercase())
            .collect::<String>()
            .parse::<char>()
            .unwrap(),
    ))
}

pub fn real_to_integer(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Int(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'REAL_TO_INTEGER' needs a real to make into an integer".to_string(),
            })?
            .to_string()
            .parse::<f32>()
            .unwrap()
            .trunc() as i32,
    ))
}

pub fn integer_to_real(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Real(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'INTEGER_TO_REAL' needs an integer to make into a real".to_string(),
            })?
            .to_string()
            .parse::<f32>()
            .map_err(|_| Error {
                span: expr.1.clone(),
                msg: "Couldn't convert to real".to_string(),
            })?,
    ))
}

pub fn asc(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Int(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'ASC' needs a character to get the ascii index".to_string(),
            })?
            .to_string()
            .chars()
            .next()
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Couldn't find the character to convert".to_string(),
            })? as i32,
    ))
}

pub fn mod_(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Int(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'MOD' needs an integer to calculate the modulus of".to_string(),
            })?
            .to_string()
            .parse::<i32>()
            .map_err(|_| Error {
                span: expr.1.clone(),
                msg: "Couldn't convert to integer".to_string(),
            })?
            % vars_func
                .get(1)
                .ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: "Builtin 'MOD' needs an integer to calculate the modulus of".to_string(),
                })?
                .to_string()
                .parse::<i32>()
                .map_err(|_| Error {
                    span: expr.1.clone(),
                    msg: "Couldn't convert to integer".to_string(),
                })?,
    ))
}

pub fn integer_to_string(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Str(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'INTEGER_TO_STRING' needs an integer to convert to string"
                    .to_string(),
            })?
            .to_string(),
    ))
}

pub fn real_to_string(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Str(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'REAL_TO_STRING' needs a real to convert to string".to_string(),
            })?
            .to_string(),
    ))
}

pub fn char_to_string(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Str(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'CHAR_TO_STRING' needs a char to convert to string".to_string(),
            })?
            .to_string(),
    ))
}

pub fn string_to_char(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Char(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'STRING_TO_CHAR' needs a string to convert to char".to_string(),
            })?
            .to_string()
            .chars()
            .next()
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Couldn't find the character to convert".to_string(),
            })?,
    ))
}

pub fn string_to_real(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Real(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'STRING_TO_REAL' needs a string to convert to real".to_string(),
            })?
            .to_string()
            .parse::<f32>()
            .map_err(|_| Error {
                span: expr.1.clone(),
                msg: "Cannot convert string to real, not a valid real".to_string(),
            })?,
    ))
}

pub fn string_to_integer(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Int(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'STRING_TO_INTEGER' needs a string to convert to integer".to_string(),
            })?
            .to_string()
            .parse::<i32>()
            .map_err(|_| Error {
                span: expr.1.clone(),
                msg: "Cannot convert string to integer, not a valid integer".to_string(),
            })?,
    ))
}

pub fn char_to_integer(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Int(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'CHAR_TO_INTEGER' needs a char to convert to integer".to_string(),
            })?
            .to_string()
            .parse::<i32>()
            .map_err(|_| Error {
                span: expr.1.clone(),
                msg: "Cannot convert char to integer, not a valid integer".to_string(),
            })?,
    ))
}

pub fn char_to_real(vars_func: &[Value], expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(Value::Real(
        vars_func
            .get(0)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: "Builtin 'CHAR_TO_REAL' needs a char to convert to real".to_string(),
            })?
            .to_string()
            .parse::<f32>()
            .map_err(|_| Error {
                span: expr.1.clone(),
                msg: "Cannot convert char to real, not a valid real".to_string(),
            })?,
    ))
}
