use std::collections::HashMap;

use crate::{
    parser::{ArgMode, BinaryOp, Expr, Value},
    prelude::*,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    args: Vec<((ArgMode, String), String)>,
    body: Box<Spanned<Expr>>,
    returns: String,
}

#[derive(Debug, Clone)]
pub enum Types {
    Composite(HashMap<String, String>),
    Enumerated(Vec<String>),
    Func(Func),
    Real,
    String,
    Integer,
    Boolean,
    Char,
    Null,
}

impl PartialEq for Types {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Composite(_) => match other {
                Self::Composite(_) => true,
                _ => false,
            },
            Self::Enumerated(_) => match other {
                Self::Enumerated(_) => true,
                _ => false,
            },
            Self::Func(_) => match other {
                Self::Func(_) => true,
                _ => false,
            },
            Self::Real => match other {
                Self::Real => true,
                _ => false,
            },
            Self::String => match other {
                Self::String => true,
                _ => false,
            },
            Self::Integer => match other {
                Self::Integer => true,
                _ => false,
            },
            Self::Boolean => match other {
                Self::Boolean => true,
                _ => false,
            },
            Self::Char => match other {
                Self::Char => true,
                _ => false,
            },
            Self::Null => match other {
                Self::Null => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl ToString for Types {
    fn to_string(&self) -> String {
        match self {
            Real => "REAL".to_string(),
            String => "STRING".to_string(),
            Integer => "INTEGER".to_string(),
            Boolean => "BOOLEAN".to_string(),
            Char => "CHAR".to_string(),
            Null => "NULL".to_string(),
            _ => "NULL".to_string(),
        }
    }
}

impl From<Value> for Types {
    fn from(v: Value) -> Self {
        match v {
            Value::Bool(_) => Self::Boolean,
            Value::Char(_) => Self::Char,
            Value::Str(_) => Self::String,
            Value::Real(_) => Self::Real,
            Value::Int(_) => Self::Integer,
            Value::Comp(_) => Self::Composite(HashMap::new()),
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
                            Value::Str(s) => Some(s),
                            Value::Null => None,
                            _ => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!(
                                        "Cannot access composite type '{}' with non string '{}'",
                                        name, key
                                    ),
                                })
                            }
                        };

                        if let Some(key) = key {
                            local_hash
                                .get(&key)
                                .ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!("Field is not initalized on '{}'", name),
                                })?
                                .to_owned()
                        } else {
                            Value::Comp(local_hash)
                        }
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
            vars.insert(name.0.clone(), (str_type.clone(), Some(rhs)));
            let output = eval(then, vars, types);
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
                            msg: format!("cannot add {} to {} due to confilcting types", a, b),
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
        Expr::Func(name, args, type_, body, then) => {
            types.insert(
                name.to_string(),
                Types::Func(Func {
                    args: args.clone(),
                    body: body.clone(),
                    returns: type_.clone(),
                }),
            );
            vars.insert(
                name.to_string(),
                (name.to_string(), Some(Value::Func(name.to_string()))),
            );
            eval(then, vars, types)?
        }
        Expr::Call(func, args) => {
            let ctypes = types.clone();
            let func = ctypes.get(&func.clone()).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("function '{}' has not been declared", func),
            })?;

            match func {
                Types::Func(f) => {
                    if args.len() != f.args.len() {
                        return Err(Error{
                            span: expr.1.clone(),
                            msg: format!("arg count mismatch between definition and call. Expected {} args got {}", f.args.len(), args.len())
                        });
                    }
                    let mut vars_func = HashMap::new();
                    let mut reference = Vec::new();
                    for (n, i) in args.iter().enumerate() {
                        let name = match &i.0 {
                            Expr::Var(n, _) => Some(n),
                            _ => None,
                        };
                        let t_def = f.args.get(n).unwrap().clone();
                        let t_type = types.get(&t_def.1).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: format!("Type '{}' is not declared", t_def.1),
                        })?;
                        let v_type = Types::from(eval(i, vars, &mut types.clone())?);

                        if *t_type != v_type {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!(
                                    "Type '{:?}' does not match type '{:?}'",
                                    t_type, v_type
                                ),
                            });
                        }

                        vars_func
                            .insert((t_def.0).1.clone(), (t_def.1, Some(eval(i, vars, types)?)));
                        match (t_def.0).0 {
                            ArgMode::Byref => {
                                if let Some(name) = name {
                                    reference.push((name, (t_def.0).1));
                                }
                            }
                            _ => {}
                        };
                    }
                    let output = eval(&f.body, &mut vars_func, types);
                    for i in reference {
                        if let Some(v) = vars_func.get(&i.1) {
                            vars.insert(i.0.to_string(), v.clone());
                        }
                    }
                    let output = output?.clone();
                    let return_type = types.get(&f.returns).unwrap();
                    if return_type.clone() == Types::from(output.clone()) {
                        output
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "Type '{:?}' does not match return type type '{:?}'",
                                Types::from(output.clone()),
                                return_type.clone()
                            ),
                        });
                    }
                }
                _ => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("identifier '{:?}' is not a function", func),
                    })
                }
            }
        }
        Expr::Return(v) => eval(v, vars, types)?,
        _ => {
            todo!()
        }
    })
}
