use std::collections::HashMap;

use crate::{
    parser::{BinaryOp, Expr, Value},
    prelude::*,
};

pub struct Error {
    pub span: Span,
    pub msg: String,
}

pub fn eval<'a>(
    expr: &Spanned<Expr>,
    vars: &mut HashMap<String, (String, Option<Value>)>,
    funcs: &mut Vec<(String, &'a [String], &'a Expr)>,
) -> Result<Value, Error> {
    Ok(match &expr.0 {
        Expr::Error => unreachable!(),
        Expr::Value(val) => val.clone(),
        Expr::Var(name) => vars
            .get(name)
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("No such variable '{}' in scope", name),
            })?
            .1
            .to_owned()
            .ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' is declared but never initialized", name),
            })?,
        Expr::DeclarePrim(name, type_, then) => {
            vars.insert(name.clone(), (type_.clone(), None));
            let output = eval(then, vars, funcs);
            vars.remove(name);
            output?
        }
        Expr::Assign(name, rhs, then) => {
            let rhs = eval(rhs, vars, funcs)?;

            let type_ = &vars.get(name).unwrap().0;

            match type_.as_str() {
                "INTEGER" => {
                    if !rhs.is_int() {
                        println!("{:?}", rhs);
                        unreachable!()
                    }
                }
                "REAL" => {
                    if !rhs.is_real() {
                        unreachable!()
                    }
                }
                "BOOLEAN" => {
                    if !rhs.is_bool() {
                        unreachable!()
                    }
                }
                "STRING" => {
                    if !rhs.is_str() {
                        unreachable!()
                    }
                }
                _ => {
                    unreachable!()
                }
            };
            vars.insert(name.clone(), (type_.clone(), Some(rhs)));
            let output = eval(then, vars, funcs);
            vars.remove(name);
            output?
        }
        Expr::Binary(a, BinaryOp::Add, b) => {
            let a = eval(a, vars, funcs)?;
            let b = eval(b, vars, funcs)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Int(a + b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot add {} to {} due to confilcting types", a, b),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Real(a + b)
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Output(val, then) => {
            println!("{}", eval(val, vars, funcs)?);
            eval(then, vars, funcs)?
        }
        _ => Value::Null,
    })
}
