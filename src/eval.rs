use std::collections::HashMap;

use crate::{
    parser::{BinaryOp, Expr, Value},
    prelude::*,
};

#[derive(PartialEq, Debug)]
pub enum Types {
    Composite(HashMap<String, String>),
    Enumerated(Vec<String>),
    Real,
    String,
    Integer,
    Boolean,
    Char,
    Null,
}

impl From<Value> for Types {
    fn from(v: Value) -> Self {
        match v {
            Value::Bool(_) => Self::Boolean,
            Value::Char(_) => Self::Char,
            Value::Str(_) => Self::String,
            Value::Real(_) => Self::Real,
            Value::Int(_) => Self::Integer,
            _ => Self::Null,
        }
    }
}

pub struct Error {
    pub span: Span,
    pub msg: String,
}

pub fn eval<'a>(
    expr: &Spanned<Expr>,
    vars: &mut HashMap<String, (String, Option<Value>)>,
    types: &mut HashMap<String, Types>,
) -> Result<Value, Error> {
    Ok(match &expr.0 {
        Expr::Error => unreachable!(),
        Expr::Value(val) => val.clone(),
        Expr::Var(name, sub) => {
            let v = vars
                .get(name)
                .ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: format!("No such variable '{}' in scope", name),
                })?
                .to_owned();

            let type_ = types.get(&v.0).unwrap();

            let raw_value = v.1.clone().ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' is declared but never initialized", name),
            })?;

            match type_ {
                Types::Composite(_) => match raw_value {
                    Value::Comp(local_hash) => {
                        let key = eval(sub, vars, types)?;
                        let key = match key {
                            Value::Str(s) => s,
                            _ => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!(
                                        "Cannot access composite type '{}' with non string",
                                        name
                                    ),
                                })
                            }
                        };
                        local_hash
                            .get(&key)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("Field is not initalized on '{}'", name),
                            })?
                            .to_owned()
                    }
                    _ => {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Type value mismatch on '{}'", name),
                        })
                    }
                },
                _ => v.1.ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: format!("Variable '{}' has not been initialized", name),
                })?,
            }
        }
        Expr::DeclarePrim(name, type_, then) => {
            vars.insert(name.clone(), (type_.clone(), None));
            let output = eval(then, vars, types);
            vars.remove(name);
            output?
        }
        Expr::Assign(name, rhs, then) => {
            let mut rhs = eval(rhs, vars, types)?;

            let (str_type, val) = &vars.get(&name.0).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("No such variable '{}' in scope", &name.0),
            })?;

            let type_ = types.get(str_type).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("No such type '{}' in scope", str_type),
            })?;

            match type_ {
                Types::Integer => {
                    if !rhs.is_int() {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Variable '{}' must be assigned to type INTEGER", name.0),
                        });
                    }
                }
                Types::Real => {
                    if !rhs.is_real() {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Variable '{}' must be assigned to type REAL", name.0),
                        });
                    }
                }
                Types::Boolean => {
                    if !rhs.is_bool() {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Variable '{}' must be assigned to type BOOLEAN", name.0),
                        });
                    }
                }
                Types::String => {
                    if !rhs.is_str() {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Variable '{}' must be assigned to type STRING", name.0),
                        });
                    }
                }

                Types::Composite(h) => {
                    let field = match &name.1 {
                        Expr::Value(s) => match s {
                            Value::Str(s) => s,
                            _ => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!(
                                        "Composite datatype '{}' cannot be accessed by value {:?}",
                                        name.0, name.1
                                    ),
                                })
                            }
                        },
                        _ => {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!(
                                    "Composite datatype '{}' cannot be accessed by expression {:?}",
                                    name.0, name.1
                                ),
                            })
                        }
                    };

                    if let Some(map) = val.clone() {
                        match map {
                            Value::Comp(mut local_h) => {
                                let t = h.get(field).ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!(
                                        "Field '{}' is not defined on this composite type",
                                        field
                                    ),
                                })?;
                                let t = types.get(t).ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!("type '{}' has not been defined", t),
                                })?;

                                if Types::from(rhs.clone()) == *t {
                                    local_h.insert(field.to_owned(), rhs);

                                    rhs = Value::Comp(local_h);
                                } else {
                                    return Err(Error {
                                        span: expr.1.clone(),
                                        msg: format!(
                                            "Cannot assign '{:?}' must be assigned to type '{:?}'",
                                            Types::from(rhs.clone()),
                                            *t
                                        ),
                                    });
                                }
                            }
                            _ => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!(
                                        "Variable '{}' must be assigned to type STRING",
                                        name.0
                                    ),
                                })
                            }
                        }
                    } else {
                        let mut map = HashMap::new();
                        let t = h.get(field).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: format!("Field '{}' is not defined on this composite type", field),
                        })?;
                        let t = types.get(t).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: format!("type '{}' has not been defined", t),
                        })?;

                        if Types::from(rhs.clone()) == *t {
                            map.insert(field.to_owned(), rhs);

                            rhs = Value::Comp(map);
                        } else {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!(
                                    "Cannot assign '{:?}' must be assigned to type '{:?}'",
                                    Types::from(rhs.clone()),
                                    *t
                                ),
                            });
                        }
                    }
                }
                _ => {
                    unreachable!()
                }
            };
            println!("{:?}", vars);
            vars.insert(name.0.clone(), (str_type.clone(), Some(rhs)));
            let output = eval(then, vars, types);
            vars.remove(&name.0);
            output?
        }
        Expr::Binary(a, BinaryOp::Add, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Int(a + b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("cannot add {} to {} due to confilcting types", a, b),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Real(a + b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("cannot mutliply {} to {} due to confilcting types", a, b),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Binary(a, BinaryOp::Eq, b) => {
            Value::Bool(eval(a, vars, types)? == eval(b, vars, types)?)
        }
        Expr::Binary(a, BinaryOp::NotEq, b) => {
            Value::Bool(eval(a, vars, types)? != eval(b, vars, types)?)
        }
        Expr::Binary(a, BinaryOp::Mul, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Int(a * b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("cannot mutliply {} to {} due to confilcting types", a, b),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Real(a * b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("cannot mutliply {} to {} due to confilcting types", a, b),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Binary(a, BinaryOp::Sub, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Int(a - b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "cannot subtract {} from {} due to confilcting types",
                                b, a
                            ),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Real(a - b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "cannot subtract {} from {} due to confilcting types",
                                b, a
                            ),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Binary(a, BinaryOp::Div, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Int(a / b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("cannot divide {} dy {} due to confilcting types", b, a),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Real(a / b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("cannot divide {} by {} due to confilcting types", b, a),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Output(val, then) => {
            for i in val {
                print!("{}", eval(i, vars, types)?);
            }
            println!();
            eval(then, vars, types)?
        }
        Expr::If(cond, a, b) => {
            let c = eval(cond, vars, types)?;
            match c {
                Value::Bool(true) => eval(a, vars, types)?,
                Value::Bool(false) => eval(b, vars, types)?,
                c => {
                    return Err(Error {
                        span: cond.1.clone(),
                        msg: format!("Conditions must be booleans, found '{:?}'", c),
                    })
                }
            }
        }
        Expr::Then(a, b) => {
            eval(a, vars, types)?;
            eval(b, vars, types)?
        }
        Expr::DeclareComp(name, items, body) => {
            let items = eval(items, vars, types)?;

            let items = match items {
                Value::Comp(h) => h,
                _ => unreachable!(),
            };

            let mut new_h = HashMap::new();

            for i in items {
                let type_ = match i.1 {
                    Value::Str(s) => s,
                    _ => unreachable!(),
                };

                new_h.insert(i.0, type_);
            }

            types.insert(name.clone(), Types::Composite(new_h));
            let output = eval(body, vars, types);
            output?
        }
        Expr::Func(name, args, type_, body) => {
            todo!()
        }
        _ => todo!(),
    })
}
