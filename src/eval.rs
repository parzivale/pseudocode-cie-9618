use std::{collections::HashMap, hash::Hash};

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
    Composite(HashMap<String, Types>),
    Enumerated(Vec<String>),
    Array(String, i32, i32),
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
            Self::Composite(_) => matches!(other, Self::Composite(_)),
            Self::Enumerated(_) => matches!(other, Self::Enumerated(_)),
            Self::Func(_) => matches!(other, Self::Func(_)),
            Self::Array(_, _, _) => matches!(other, Self::Array(_, _, _)),
            Self::Real => matches!(other, Self::Real),
            Self::String => matches!(other, Self::String),
            Self::Integer => matches!(other, Self::Integer),
            Self::Boolean => matches!(other, Self::Boolean),
            Self::Char => matches!(other, Self::Char),
            Self::Null => matches!(other, Self::Null),
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
            Value::Comp(h) => {
                let mut map = HashMap::new();
                for i in h {
                    map.insert(i.0, Types::from(i.1));
                }
                Self::Composite(map)
            }
            _ => Self::Null,
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}

pub fn eval(
    expr: &Spanned<Expr>,
    vars: &mut HashMap<String, (String, Option<Value>)>,
    types: &mut HashMap<String, Types>,
) -> Result<Value, Error> {
    //println!("{:?}\n\n{:?}\n", vars, types);
    Ok(match &expr.0 {
        Expr::Error => unreachable!(),
        Expr::Value(val) => val.clone(),
        Expr::Var(name, sub) => {
            let name = match eval(name, vars, types)? {
                Value::Str(s) => s,
                n => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("No such variable '{}' in scope", n),
                    })
                }
            };

            let v = vars
                .get(&name)
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
                Types::Func(_) => {
                    eval(&(Expr::Call(v.0, Vec::new()), expr.1.clone()), vars, types)?
                }
                Types::Array(_, _, _) => match raw_value {
                    Value::Array(v) => {
                        let index = match eval(sub, vars, types)? {
                            Value::Int(i) => i,
                            t => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!("Cannot index array with value '{}'", t),
                                })
                            }
                        };

                        v.get(&index)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("Value at index '{}' is not defiened", index),
                            })?
                            .to_owned()
                    }
                    t => {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Expected Array got type'{}'", t),
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
        Expr::Assign(name, children, rhs, then) => {
            let rhs = eval(rhs, vars, types)?;

            let name = match eval(name, vars, types)? {
                Value::Str(s) => s,
                v => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("Cannot use '{}' as a variable", v),
                    })
                }
            };

            let (type_str_, _) = vars.get(&name).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("'{}' Has not been declared", name),
            })?;

            let type_ = types.get(type_str_).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("'{}' Has not been defined", type_str_),
            })?;

            match type_ {
                Types::Composite(h) => todo!(),
                // if the value isnt a user defined type, just assign it directly
                t => {
                    if *t == Types::from(rhs.clone()) {
                        vars.insert(name, (type_str_.to_string(), Some(rhs)));
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "Cannot assign type '{:?}' to type '{:?}'",
                                *t,
                                Types::from(rhs)
                            ),
                        });
                    }
                }
            };

            eval(then, vars, types)?;
            Value::Null
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
                            msg: format!("Cannot add {} to {} due to confilcting types", a, b),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Real(a + b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot add {} to {} due to confilcting types", a, b),
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
                        Value::Real(a as f32 / b as f32)
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
        Expr::Binary(a, BinaryOp::Ge, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Bool(a > b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Bool(a > b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Binary(a, BinaryOp::Geq, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Bool(a >= b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Bool(a >= b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Binary(a, BinaryOp::Le, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Bool(a < b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Bool(a < b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }
        Expr::Binary(a, BinaryOp::Leq, b) => {
            let a = eval(a, vars, types)?;
            let b = eval(b, vars, types)?;

            match a {
                Value::Int(a) => {
                    if let Value::Int(b) = b {
                        Value::Bool(a <= b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
                        });
                    }
                }
                Value::Real(a) => {
                    if let Value::Real(b) = b {
                        Value::Bool(a <= b)
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Cannot compare '{}' to '{}'", b, a),
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
        Expr::If(cond, a, b, body) => {
            let c = eval(cond, vars, types)?;
            match c {
                Value::Bool(true) => match eval(a, vars, types)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    _ => return eval(body, vars, types),
                },
                Value::Bool(false) => match eval(b, vars, types)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    _ => return eval(body, vars, types),
                },
                c => {
                    return Err(Error {
                        span: (**cond).1.clone(),
                        msg: format!("Conditions must be booleans, found '{:?}'", c),
                    })
                }
            }
        }
        Expr::DeclareComp(name, items, body) => {
            let mut new_v = HashMap::new();
            eval(items, &mut new_v, types)?;
            let mut map = HashMap::new();
            for i in new_v {
                let type_ = types.get(&(i.1).0).ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: format!("Type '{}' has not been defined", (i.1).0),
                })?;
                map.insert(i.0, type_.clone());
            }
            types.insert(name.clone(), Types::Composite(map));
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
        Expr::ProcCall(expr, then) => {
            eval(expr, vars, types)?;
            eval(then, vars, types)?
        }
        Expr::Call(func, args) => {
            //gets the function definition from the type table
            let ctypes = types.clone();
            let func = ctypes.get(&func.clone()).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("function '{}' has not been declared", func),
            })?;

            //makes sure what we got back was a function
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
                    //type check each argument
                    for (n, i) in args.iter().enumerate() {
                        let name = match &i.0 {
                            Expr::Var(n, _) => Some(n),
                            _ => None,
                        };
                        // checks if type exsists
                        let t_def = f.args.get(n).unwrap().clone();
                        let t_type = types.get(&t_def.1).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: format!("Type '{}' is not declared", t_def.1),
                        })?;
                        let v_type = Types::from(eval(i, vars, &mut types.clone())?);

                        //checks if types dont match
                        if *t_type != v_type {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!(
                                    "Type '{:?}' does not match type '{:?}'",
                                    t_type, v_type
                                ),
                            });
                        }

                        //store the vars on a local var map
                        vars_func
                            .insert((t_def.0).1.clone(), (t_def.1, Some(eval(i, vars, types)?)));

                        // store whether the variable was passed byref
                        if (t_def.0).0 == ArgMode::Byref {
                            if let Some(name) = name {
                                reference.push((name, (t_def.0).1));
                            }
                        }
                    }
                    let output = eval(&f.body, &mut vars_func, types);

                    // update the variables that where passed byref

                    for i in reference {
                        let name = match eval(i.0, vars, types)? {
                            Value::Str(s) => s,
                            n => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!("No such variable '{}' in scope", n),
                                })
                            }
                        };

                        if let Some(v) = vars_func.get(&i.1) {
                            vars.insert(name.to_string(), v.clone());
                        }
                    }

                    let res = output?;
                    let output = match res {
                        Value::Return(t) => *t,
                        Value::Null => Value::Null,
                        v => {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!("Value '{}' is not a return type", v),
                            })
                        }
                    };

                    let return_type = types.get(&f.returns).unwrap();
                    if return_type.clone() == Types::from(output.clone()) {
                        output
                    } else {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "Type '{:?}' does not match return type type '{:?}'",
                                Types::from(output),
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
        Expr::Return(v) => Value::Return(Box::new(eval(v, vars, types)?)),
        Expr::DeclareArr(name, start, end, type_, then) => {
            let start = match eval(start, vars, types)? {
                Value::Int(i) => i,
                v => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("Start of the array must be an integer, found '{}'", v),
                    })
                }
            };
            let end = match eval(end, vars, types)? {
                Value::Int(i) => i,
                v => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("End of the array must be an integer, found '{}'", v),
                    })
                }
            };

            types.insert(name.clone(), Types::Array(type_.clone(), start, end));
            vars.insert(name.clone(), (name.clone(), None));
            let output = eval(then, vars, types);
            //types.remove(name);
            //vars.remove(name);
            output?
        }
        Expr::While(cond, body, then) => {
            while eval(cond, vars, types)? == Value::Bool(true) {
                match eval(body, vars, types)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    v => v,
                };
            }

            eval(then, vars, types)?
        }
        _ => {
            todo!()
        }
    })
}
