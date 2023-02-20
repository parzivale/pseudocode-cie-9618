use core::panic;
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
                //for i in h {
                //    map.insert(i.0, Types::from((i.1).1.unwrap()).to_string());
                //}
                Self::Composite(map)
            }
            _ => Self::Null,
        }
    }
}

fn to_type_map(
    map: HashMap<String, String>,
    types: &mut HashMap<String, Types>,
    expr: &Spanned<Expr>,
) -> Result<HashMap<String, Types>, Error> {
    let mut new_map = HashMap::new();
    for i in map {
        let t = types.get(&i.1).ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!(
                "Cannot convert from referenced typemap to typemap due to '{}'",
                i.1
            ),
        })?;
        new_map.insert(i.1, t.clone());
    }

    Ok(new_map)
}

fn comp_fill(
    map: HashMap<String, String>,
    types: &mut HashMap<String, Types>,
    rhs: Value,
    expr: &(Expr, std::ops::Range<usize>),
    last: String,
) -> Result<HashMap<String, (String, Option<Value>)>, Error> {
    let mut temp_types = HashMap::new();
    for i in map.clone() {
        // if types e.xsists locally then continue local expansion
        match types.get(&i.1) {
            Some(t) => match t {
                Types::Composite(h) => {
                    for _ in h {
                        match types.get(&i.1) {
                            Some(Types::Composite(_)) => {
                                temp_types.insert(
                                    i.0.clone(),
                                    (
                                        i.1.clone(),
                                        Some(Value::Comp(comp_fill(
                                            h.clone(),
                                            &mut types.clone(),
                                            rhs.clone(),
                                            expr,
                                            last.clone(),
                                        )?)),
                                    ),
                                );
                            }
                            _ => {
                                if i.0.clone() == last {
                                    temp_types
                                        .insert(i.0.clone(), (i.1.clone(), Some(rhs.clone())));
                                } else {
                                    temp_types.insert(i.0.clone(), (i.1.clone(), None));
                                }
                            }
                        }
                    }
                }
                Types::Array(t, s, e) => {
                    let mut temp = HashMap::new();
                    match types.get(t) {
                        Some(Types::Composite(h)) => {
                            for i in *s..*e {
                                temp.insert(
                                    i.to_string(),
                                    (
                                        t.clone(),
                                        Some(Value::Comp(comp_fill(
                                            h.clone(),
                                            &mut types.clone(),
                                            rhs.clone(),
                                            expr,
                                            last.clone(),
                                        )?)),
                                    ),
                                );
                            }
                        }
                        Some(Types::Array(t, s, e)) => {
                            match types.get(t) {
                                Some(Types::Composite(h)) => {
                                    for i in *s..*e {
                                        temp.insert(
                                            i.to_string(),
                                            (
                                                t.clone(),
                                                Some(Value::Comp(comp_fill(
                                                    h.clone(),
                                                    &mut types.clone(),
                                                    rhs.clone(),
                                                    expr,
                                                    last.clone(),
                                                )?)),
                                            ),
                                        );
                                    }
                                }
                                Some(Types::Array(t, s, e)) => {
                                    let mut h = HashMap::new();
                                    for i in *s..*e {
                                        h.insert(i.to_string(), t.clone());
                                    }

                                    for i in *s..*e {
                                        temp.insert(
                                            i.to_string(),
                                            (
                                                t.clone(),
                                                Some(Value::Comp(comp_fill(
                                                    h.clone(),
                                                    &mut types.clone(),
                                                    rhs.clone(),
                                                    expr,
                                                    last.clone(),
                                                )?)),
                                            ),
                                        );
                                    }
                                }
                                _ => {
                                    for i in *s..*e {
                                        temp.insert(i.to_string(), (t.clone(), None::<Value>));
                                    }
                                }
                            };
                        }
                        _ => {
                            for i in *s..*e {
                                temp.insert(i.to_string(), (t.clone(), None::<Value>));
                            }
                        }
                    }
                    temp_types.insert(i.0.clone(), (i.1.clone(), Some(Value::Comp(temp))));
                }
                _ => {
                    if i.0.clone() == last {
                        temp_types.insert(i.0.clone(), (i.1.clone(), Some(rhs.clone())));
                    } else {
                        temp_types.insert(i.0.clone(), (i.1.clone(), None));
                    }
                }
            },
            _ => panic!(),
        };
    }

    Ok(temp_types)
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
    //println!("vars:{:?}\n\ntypes:{:?}\n", vars, types);
    Ok(match &expr.0 {
        Expr::Error => unreachable!(),
        Expr::Value(val) => val.clone(),
        Expr::Var(name) => {
            let name = match eval(name, vars, types)? {
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

            let v = vars
                .get(&name)
                .ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: format!("No such variable '{:?}' in scope (string error)", name),
                })?
                .to_owned();

            if let Some(type_) = types.get(&v.0) {
                match type_ {
                    Types::Func(_) => {
                        eval(&(Expr::Call(v.0, Vec::new()), expr.1.clone()), vars, types)?
                    }
                    _ => v.1.ok_or_else(|| Error {
                        span: expr.1.clone(),
                        msg: format!("Variable '{}' has not been initialized", name),
                    })?,
                }
            } else {
                v.1.ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: format!("Variable '{}' has not been initialized", name),
                })?
            }
        }

        Expr::CompVar(name, sub) => {
            match eval(name, vars, types)? {
                Value::Comp(h) => {
                    //println!("{:?}\n{:?}", h, sub);
                    eval(sub, &mut h.clone(), types)?
                }
                _ => panic!(),
            }
        }
        Expr::DeclarePrim(name, type_, then) => {
            // all primitive variables in pseudocode are initialized to a none type
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
                    //temporay type map to find the final type
                    let mut temp_types = h.clone();
                    let mut final_type = type_;

                    for i in children {
                        let i = eval(i, vars, &mut to_type_map(temp_types.clone(), types, expr)?)?;

                        //composite types can only be accessed by ints or strings
                        match i {
                            Value::Str(s) => {
                                let child_type = temp_types.get(&s).ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!(
                                        "Child '{}' does not exsist on composite type '{:?}'",
                                        s, temp_types
                                    ),
                                })?;

                                match types.get(child_type) {
                                    Some(Types::Composite(h)) => {
                                        final_type = Types::Composite(h.clone());
                                        temp_types = h.clone()
                                    }
                                    Some(Types::Array(t, s, e)) => {
                                        let type_ = types.get(t).unwrap();
                                        final_type = type_.clone();
                                        match type_ {
                                            Types::Composite(h) => temp_types = h.clone(),
                                            _ => {}
                                        }
                                    }
                                    Some(t) => {
                                        final_type = t.clone();
                                    }
                                    _ => panic!(),
                                };
                            }
                            //we dont need to handle array indexing here, we only care about the type
                            Value::Int(_) => {}
                            v => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!("Cannot access with composite type '{}'", v),
                                })
                            }
                        };
                    }

                    //composites cant have children of type null
                    if final_type == Types::Null {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "'{}' seems to have a NULL type, which is impossible",
                                name
                            ),
                        });
                    }

                    // check if the rhs has the same type as the final type
                    if Types::from(rhs.clone()) != final_type {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "Cannot assign '{:?}' to '{:?}'",
                                Types::from(rhs.clone()),
                                final_type
                            ),
                        });
                    }

                    // updating the var_map
                    let current_var = vars.get(&name).unwrap();
                    let mut maps = Vec::new();
                    if let Some(mut current_var) = current_var.1.clone() {
                        for i in children {
                            let i = eval(i, vars, types)?;
                            match i {
                                Value::Str(s) => match current_var.clone() {
                                    Value::Comp(h) => {
                                        maps.push(current_var);
                                        current_var = match h.get(&s).clone().unwrap().clone().1 {
                                            Some(t) => t,
                                            _ => break,
                                        };
                                    }
                                    v => maps.push(v),
                                },
                                Value::Int(i) => match current_var.clone() {
                                    // check if child is a array
                                    Value::Comp(h) => {
                                        maps.push(current_var);
                                        current_var = match h
                                            .get(&i.to_string())
                                            .clone()
                                            .unwrap()
                                            .clone()
                                            .1
                                        {
                                            Some(t) => t,
                                            _ => break,
                                        };
                                    }
                                    v => maps.push(v),
                                },
                                v => {
                                    return Err(Error {
                                        span: expr.1.clone(),
                                        msg: format!("Cannot access with composite type '{}'", v),
                                    })
                                }
                            };
                        }

                        let mut map = rhs;
                        let mut comb = maps.iter().rev().zip(children.iter().rev());
                        for (i, index) in comb {
                            let index = eval(
                                index,
                                vars,
                                &mut to_type_map(temp_types.clone(), types, expr)?,
                            )?;
                            match index {
                                Value::Str(s) => {
                                    match i {
                                        Value::Comp(h) => {
                                            let type_ = h.get(&s).unwrap().clone().0;
                                            let mut h = h.clone();
                                            h.insert(s, (type_, Some(map)));
                                            map = Value::Comp(h);
                                        }
                                        _ => panic!(),
                                    };
                                }
                                Value::Int(i_) => {
                                    match i {
                                        Value::Comp(h) => {
                                            let type_ = h.get(&i_.to_string()).unwrap().clone().0;
                                            let mut h = h.clone();
                                            h.insert(i_.to_string(), (type_, Some(map)));
                                            map = Value::Comp(h);
                                        }
                                        _ => panic!(),
                                    };
                                }
                                _ => panic!(),
                            };
                        }

                        vars.insert(name.clone(), (type_str_, Some(map)));

                    // if the variable is not assigned a value we can just create a new value
                    // this section might be more apt in declare prim but for now this is
                    // the best place for it
                    } else {
                        if let Value::Comp(h) = rhs {
                            vars.insert(name.clone(), (type_str_, Some(Value::Comp(h))));
                        } else {
                            let last = match eval(children.last().unwrap(), vars, types)? {
                                Value::Str(s) => s,
                                Value::Int(i) => i.to_string(),
                                _ => panic!(),
                            };
                            vars.insert(
                                name.clone(),
                                (
                                    type_str_,
                                    Some(Value::Comp(comp_fill(
                                        h.clone(),
                                        &mut types.clone(),
                                        rhs,
                                        &expr.clone(),
                                        last,
                                    )?)),
                                ),
                            );
                        }

                        //println!("{:?}", vars.get(&name.clone()).unwrap());
                    }

                    // end of composite type actions
                }
                Types::Array(type_, start, end) => {
                    let types_clone = types.clone();
                    let t = types_clone.get(&type_).unwrap();
                    if *t != Types::from(rhs.clone()) {
                        panic!()
                    }

                    if let Some(current_var) = vars.get(&name) {
                    } else {
                        let last = match eval(children.last().unwrap(), vars, types)? {
                            Value::Str(s) => s,
                            _ => panic!(),
                        };
                        match t {
                            Types::Composite(h) => vars.insert(
                                name.clone(),
                                (
                                    type_,
                                    Some(Value::Comp(comp_fill(
                                        h.clone(),
                                        &mut types.clone(),
                                        rhs,
                                        &expr.clone(),
                                        last,
                                    )?)),
                                ),
                            ),
                            _ => panic!(),
                        };
                    }
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

            eval(then, vars, types)?
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
            println!("{:?}", val);
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
                map.insert(i.0, (i.1).0);
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
                            Expr::Var(n) => Some(n),
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
