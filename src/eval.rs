use std::{collections::HashMap, ops::Range};

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
            Self::Real => "REAL".to_string(),
            Self::String => "STRING".to_string(),
            Self::Integer => "INTEGER".to_string(),
            Self::Boolean => "BOOLEAN".to_string(),
            Self::Char => "CHAR".to_string(),
            Self::Null => "NULL".to_string(),
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

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}

fn var(
    expr: &Spanned<Expr>,
    vars: &mut HashMap<String, (String, Option<Value>)>,
    local_vars: &mut HashMap<String, (String, Option<Value>)>,
    types: &mut HashMap<String, Types>,
    name: &Box<(Expr, Range<usize>)>,
) -> Result<Value, Error> {
    let name = match eval(name, vars, local_vars, types)? {
        Value::Str(s) => s,
        Value::Int(i) => i.to_string(),
        n => {
            return Err(Error {
                span: expr.1.clone(),
                msg: format!("No such variable '{:?}' in scope (Value error)", n),
            })
        }
    };
    //println!("vars:{:?}\nname:{:?}\ntypes:{:?}", vars, name, types);

    let v = local_vars
        .get(&name)
        .ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("No such variable '{:?}' in scope (string error)", name),
        })
        .or_else(|err| vars.get(&name).ok_or(err))?
        .to_owned();

    if let Some(type_) = types.get(&v.0) {
        match type_ {
            Types::Func(_) => Ok(eval(
                &(Expr::Call(v.0, Vec::new()), expr.1.clone()),
                vars,
                local_vars,
                types,
            )?),
            _ => Ok(v.1.ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' has not been initialized", name),
            })?),
        }
    } else {
        Ok(v.1.ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("Variable '{}' has not been initialized", name),
        })?)
    }
}

fn comp_var(
    expr: &Spanned<Expr>,
    vars: &mut HashMap<String, (String, Option<Value>)>,
    local_vars: &mut HashMap<String, (String, Option<Value>)>,
    types: &mut HashMap<String, Types>,
    name: &Box<(Expr, Range<usize>)>,
    sub: &Box<(Expr, Range<usize>)>,
) -> Result<Value, Error> {
    Ok(match eval(name, vars, local_vars, types)? {
        Value::Comp(h) => {
            //println!("{:?}\n{:?}", h, sub);
            eval(sub, vars, &mut h.clone(), types)?
        }
        v => {
            return Err(Error {
                span: expr.1.clone(),
                msg: format!("Unexpected value '{:?}'", v),
            })
        }
    })
}

fn declare_prim(
    vars: &mut HashMap<String, (String, Option<Value>)>,
    local_vars: &mut HashMap<String, (String, Option<Value>)>,
    types: &mut HashMap<String, Types>,
    name: &str,
    type_: &str,
    then: &Box<(Expr, Range<usize>)>,
) -> Result<Value, Error> {
    // all primitive variables in pseudocode are initialized to a none type
    vars.insert(name.to_string(), (type_.to_string(), None));
    eval(then, vars, local_vars, types)
}

#[allow(clippy::too_many_arguments)]
fn assign(
    expr: &Spanned<Expr>,
    vars: &mut HashMap<String, (String, Option<Value>)>,
    local_vars: &mut HashMap<String, (String, Option<Value>)>,
    types: &mut HashMap<String, Types>,
    name: &Box<(Expr, Range<usize>)>,
    children: &Vec<(Expr, Range<usize>)>,
    rhs: &Box<(Expr, Range<usize>)>,
    then: &Box<(Expr, Range<usize>)>,
) -> Result<Value, Error> {
    let rhs = eval(rhs, vars, local_vars, types)?;
    let name = match eval(name, vars, local_vars, types)? {
        Value::Str(s) => s,
        v => {
            return Err(Error {
                span: expr.1.clone(),
                msg: format!("Cannot use '{}' as a variable", v),
            })
        }
    };

    // type of the lhs as string
    let (type_str_, _) = vars
        .get(&name)
        .ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("'{}' Has not been declared", name),
        })?
        .clone();

    let type_ = types
        .get(&type_str_)
        .ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("'{}' Has not been defined", type_str_),
        })?
        .clone();

    // different actions for different types
    match type_.clone() {
        Types::Composite(h) => {
            //temporary type map to find the final type
            let temp_types = h.clone();
            let final_type = type_;

            let temp_types = type_check_comp(
                children, vars, local_vars, types, temp_types, expr, final_type, &rhs, &name,
            )?;

            // updating the var_map
            let current_var = vars.get(&name).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' has not been delcared", name),
            })?;

            update_comp_vars(
                current_var.clone(),
                children,
                vars,
                local_vars,
                types,
                expr,
                temp_types,
                rhs,
                name,
                type_str_,
                h,
            )?;

            // end of composite type actions
        }
        Types::Array(type_string_, start, end) => {
            let mut h = HashMap::new();
            for i in start..end {
                h.insert(i.to_string(), type_string_.clone());
            }

            let temp_types = h.clone();
            let final_type = type_;

            let temp_types = type_check_comp(
                children, vars, local_vars, types, temp_types, expr, final_type, &rhs, &name,
            )?;

            let current_var = vars.get(&name).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' has not been declared", name),
            })?;

            update_comp_vars(
                current_var.clone(),
                children,
                vars,
                local_vars,
                types,
                expr,
                temp_types,
                rhs,
                name,
                type_str_,
                h,
            )?;
        }
        // if the variable isnt a user defined type, just assign it directly
        t => {
            if t == Types::from(rhs.clone()) {
                vars.insert(name, (type_str_, Some(rhs)));
            } else {
                return Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Cannot assign type '{:?}' to type '{:?}'",
                        t,
                        Types::from(rhs)
                    ),
                });
            }
        }
    };

    eval(then, vars, local_vars, types)
}

pub fn eval(
    expr: &Spanned<Expr>,
    vars: &mut HashMap<String, (String, Option<Value>)>,
    local_vars: &mut HashMap<String, (String, Option<Value>)>,
    types: &mut HashMap<String, Types>,
) -> Result<Value, Error> {
    //println!("vars:{:?}\n\ntypes:{:?}\n", vars, types);
    Ok(match &expr.0 {
        Expr::Error => unreachable!(),
        Expr::Value(val) => val.clone(),
        Expr::Var(name) => var(expr, vars, local_vars, types, name)?,
        Expr::CompVar(name, sub) => comp_var(expr, vars, local_vars, types, name, sub)?,
        Expr::DeclarePrim(name, type_, then) => {
            declare_prim(vars, local_vars, types, name, type_, then)?
        }
        Expr::Assign(name, children, rhs, then) => {
            assign(expr, vars, local_vars, types, name, children, rhs, then)?
        }
        Expr::Binary(a, BinaryOp::Add, b) => {
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
            Value::Bool(eval(a, vars, local_vars, types)? == eval(b, vars, local_vars, types)?)
        }
        Expr::Binary(a, BinaryOp::NotEq, b) => {
            Value::Bool(eval(a, vars, local_vars, types)? != eval(b, vars, local_vars, types)?)
        }
        Expr::Binary(a, BinaryOp::Mul, b) => {
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
            let a = eval(a, vars, local_vars, types)?;
            let b = eval(b, vars, local_vars, types)?;

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
                print!("{}", eval(i, vars, local_vars, types)?);
            }
            println!();
            eval(then, vars, local_vars, types)?
        }
        Expr::If(cond, a, b, body) => {
            let c = eval(cond, vars, local_vars, types)?;
            match c {
                Value::Bool(true) => match eval(a, vars, local_vars, types)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    _ => return eval(body, vars, local_vars, types),
                },
                Value::Bool(false) => match eval(b, vars, local_vars, types)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    _ => return eval(body, vars, local_vars, types),
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
            eval(items, &mut new_v, local_vars, types)?;
            let mut map = HashMap::new();
            for i in new_v {
                map.insert(i.0, (i.1).0);
            }
            types.insert(name.clone(), Types::Composite(map));
            let output = eval(body, vars, local_vars, types);
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
            eval(then, vars, local_vars, types)?
        }
        Expr::ProcCall(expr, then) => {
            eval(expr, vars, local_vars, types)?;
            eval(then, vars, local_vars, types)?
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
                            Expr::Var(n) => Some(n),
                            _ => None,
                        };
                        // checks if type exsists
                        let t_def = f
                            .args
                            .get(n)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("No arg found at index '{}'", n),
                            })?
                            .clone();
                        let t_type = types.get(&t_def.1).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: format!("Type '{}' is not declared", t_def.1),
                        })?;
                        let v_type = Types::from(eval(i, vars, local_vars, &mut types.clone())?);

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
                        vars_func.insert(
                            (t_def.0).1.clone(),
                            (t_def.1, Some(eval(i, vars, local_vars, types)?)),
                        );

                        // store whether the variable was passed byref
                        if (t_def.0).0 == ArgMode::Byref {
                            if let Some(name) = name {
                                reference.push((name, (t_def.0).1));
                            }
                        }
                    }
                    let output = eval(&f.body, &mut vars_func, local_vars, types);

                    // update the variables that where passed byref

                    for i in reference {
                        let name = match eval(i.0, vars, local_vars, types)? {
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

                    let return_type = types.get(&f.returns).ok_or_else(|| Error {
                        span: expr.1.clone(),
                        msg: format!("Type '{}' has not been declared", f.returns),
                    })?;
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
        Expr::Return(v) => Value::Return(Box::new(eval(v, vars, local_vars, types)?)),
        Expr::DeclareArr(name, start, end, type_, then) => {
            let start = match eval(start, vars, local_vars, types)? {
                Value::Int(i) => i,
                v => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("Start of the array must be an integer, found '{}'", v),
                    })
                }
            };
            let end = match eval(end, vars, local_vars, types)? {
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
            let output = eval(then, vars, local_vars, types);
            //types.remove(name);
            //vars.remove(name);
            output?
        }
        Expr::While(cond, body, then) => {
            while eval(cond, vars, local_vars, types)? == Value::Bool(true) {
                match eval(body, vars, local_vars, types)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    v => v,
                };
            }

            eval(then, vars, local_vars, types)?
        }
        _ => {
            todo!()
        }
    })
}
