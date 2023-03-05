use std::{
    collections::HashMap,
    sync::{mpsc::*, Arc, Mutex},
    thread,
};

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

#[derive(Debug, Clone, PartialEq)]
pub struct Builtin {
    pub args: Vec<String>,
    pub returns: String,
}

#[derive(Debug, Clone)]
pub enum Types {
    Composite(HashMap<String, String>),
    Enumerated(Vec<String>),
    Array(String, i32, i32),
    Func(Func),
    Builtin(Builtin),
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
            Self::Builtin(_) => matches!(other, Self::Builtin(_)),
            Self::Array(_, _, _) => matches!(other, Self::Array(_, _, _)),
            Self::Real => matches!(other, Self::Real),
            Self::String => matches!(other, Self::String),
            Self::Integer => matches!(other, Self::Integer),
            Self::Boolean => matches!(other, Self::Boolean),
            Self::Char => matches!(other, Self::Char),
            Self::Null => matches!(other, Self::Null),
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

pub type VarMap = HashMap<String, (String, Option<Value>)>;
pub type TypeMap = HashMap<String, Types>;

pub struct Ctx {
    pub vars: VarMap,
    pub local_vars: VarMap,
    pub types: TypeMap,
    pub channel: Sender<Actions>,
    pub input: Arc<Mutex<String>>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            local_vars: HashMap::new(),
            types: HashMap::new(),
            channel: channel().0,
            input: Arc::new(Mutex::new("".to_string())),
        }
    }
}

fn var(expr: &Spanned<Expr>, ctx: &mut Ctx, name: &Spanned<Expr>) -> Result<Value, Error> {
    let name = match eval(name, ctx)? {
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

    let v = ctx
        .local_vars
        .get(&name)
        .ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("No such variable '{:?}' in scope (string error)", name),
        })
        .or_else(|err| ctx.vars.get(&name).ok_or(err))?
        .to_owned();

    if let Some(type_) = ctx.types.get(&v.0) {
        match type_ {
            Types::Func(_) => Ok(eval(&(Expr::Call(v.0, Vec::new()), expr.1.clone()), ctx)?),
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
    ctx: &mut Ctx,
    name: &Spanned<Expr>,
    sub: &Spanned<Expr>,
) -> Result<Value, Error> {
    Ok(match eval(name, ctx)? {
        Value::Comp(h) => {
            //println!("{:?}\n{:?}", h, sub);
            let mut ctx = Ctx {
                vars: ctx.vars.clone(),
                local_vars: h,
                types: ctx.types.clone(),
                channel: ctx.channel.clone(),
                input: Arc::clone(&ctx.input),
            };

            eval(sub, &mut ctx)?
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
    ctx: &mut Ctx,
    name: &str,
    type_: &str,
    then: &Spanned<Expr>,
) -> Result<Value, Error> {
    // all primitive variables in pseudocode are initialized to a none type
    ctx.vars.insert(name.to_string(), (type_.to_string(), None));
    eval(then, ctx)
}

#[allow(clippy::too_many_arguments)]
fn assign(
    expr: &Spanned<Expr>,
    ctx: &mut Ctx,
    name: &Spanned<Expr>,
    children: &Vec<Spanned<Expr>>,
    rhs: &Spanned<Expr>,
    then: &Spanned<Expr>,
) -> Result<Value, Error> {
    let rhs = eval(rhs, ctx)?;
    let name = match eval(name, ctx)? {
        Value::Str(s) => s,
        v => {
            return Err(Error {
                span: expr.1.clone(),
                msg: format!("Cannot use '{}' as a variable", v),
            })
        }
    };

    // type of the lhs as string

    let (type_str_, _) = ctx
        .vars
        .get(&name)
        .ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("'{}' Has not been declared", name),
        })?
        .clone();

    let type_ = ctx
        .types
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

            let temp_types =
                type_check_comp(children, ctx, temp_types, expr, final_type, &rhs, &name)?;

            // updating the var_map
            let current_var = ctx.vars.get(&name).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' has not been delcared", name),
            })?;

            update_comp_vars(
                current_var.clone(),
                children,
                ctx,
                expr,
                temp_types,
                rhs,
                name,
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

            let temp_types =
                type_check_comp(children, ctx, temp_types, expr, final_type, &rhs, &name)?;

            let current_var = ctx.vars.get(&name).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' has not been declared", name),
            })?;

            update_comp_vars(
                current_var.clone(),
                children,
                ctx,
                expr,
                temp_types,
                rhs,
                name,
                h,
            )?;
        }
        // if the variable isnt a user defined type, just assign it directly
        t => {
            if t == Types::from(rhs.clone()) {
                ctx.vars.insert(name, (type_str_, Some(rhs)));
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

    eval(then, ctx)
}

pub fn eval(expr: &Spanned<Expr>, ctx: &mut Ctx) -> Result<Value, Error> {
    //println!("vars:{:?}\n\ntypes:{:?}\n", ctx.vars, ctx.types);
    Ok(match &expr.0 {
        Expr::Error => unreachable!(),
        Expr::Value(val) => val.clone(),
        Expr::Var(name) => var(expr, ctx, name)?,
        Expr::CompVar(name, sub) => comp_var(expr, ctx, name, sub)?,
        Expr::DeclarePrim(name, type_, then) => declare_prim(ctx, name, type_, then)?,
        Expr::Assign(name, children, rhs, then) => assign(expr, ctx, name, children, rhs, then)?,
        Expr::Binary(a, BinaryOp::Add, b) => {
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
        Expr::Binary(a, BinaryOp::Eq, b) => Value::Bool(eval(a, ctx)? == eval(b, ctx)?),
        Expr::Binary(a, BinaryOp::NotEq, b) => Value::Bool(eval(a, ctx)? != eval(b, ctx)?),
        Expr::Binary(a, BinaryOp::Mul, b) => {
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
            let a = eval(a, ctx)?;
            let b = eval(b, ctx)?;

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
                let out = eval(i, ctx)?;
                ctx.channel
                    .send(Actions::Output(format!("{out} ")))
                    .unwrap();
            }
            ctx.channel.send(Actions::Output("\n".to_string())).unwrap();
            eval(then, ctx)?
        }
        Expr::If(cond, a, b, body) => {
            let c = eval(cond, ctx)?;
            match c {
                Value::Bool(true) => match eval(a, ctx)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    _ => return eval(body, ctx),
                },
                Value::Bool(false) => match eval(b, ctx)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    _ => return eval(body, ctx),
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
            let mut ctx_temp = Ctx {
                local_vars: ctx.local_vars.clone(),
                types: ctx.types.clone(),
                vars: HashMap::new(),
                channel: ctx.channel.clone(),
                input: Arc::clone(&ctx.input),
            };
            eval(items, &mut ctx_temp)?;
            let mut map = HashMap::new();
            for i in ctx_temp.vars {
                map.insert(i.0, (i.1).0);
            }
            ctx.types.insert(name.clone(), Types::Composite(map));
            let output = eval(body, ctx);
            output?
        }
        Expr::Func(name, args, type_, body, then) => {
            ctx.types.insert(
                name.to_string(),
                Types::Func(Func {
                    args: args.clone(),
                    body: body.clone(),
                    returns: type_.clone(),
                }),
            );
            ctx.vars.insert(
                name.to_string(),
                (name.to_string(), Some(Value::Func(name.to_string()))),
            );
            eval(then, ctx)?
        }
        Expr::ProcCall(expr, then) => {
            eval(expr, ctx)?;
            eval(then, ctx)?
        }
        Expr::Call(func_string, args) => {
            //gets the function definition from the type table
            let ctypes = ctx.types.clone();
            let func = ctypes.get(&func_string.clone()).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("function '{}' has not been declared", func_string),
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
                        let t_type = ctx
                            .types
                            .get(&t_def.1)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("Type '{}' is not declared", t_def.1),
                            })?
                            .clone();

                        let v_type = Types::from(eval(i, ctx)?);

                        //checks if types dont match
                        if t_type != v_type {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!(
                                    "Type '{:?}' does not match type '{:?}'",
                                    t_type, v_type
                                ),
                            });
                        }

                        //store the vars on a local var map
                        vars_func.insert((t_def.0).1.clone(), (t_def.1, Some(eval(i, ctx)?)));

                        // store whether the variable was passed byref
                        if (t_def.0).0 == ArgMode::Byref {
                            if let Some(name) = name {
                                reference.push((name, (t_def.0).1));
                            }
                        }
                    }

                    let mut temp_ctx = Ctx {
                        vars: vars_func,
                        local_vars: ctx.local_vars.clone(),
                        types: ctx.types.clone(),
                        channel: ctx.channel.clone(),
                        input: Arc::clone(&ctx.input),
                    };

                    let output = eval(&f.body, &mut temp_ctx);

                    // update the variables that where passed byref

                    for i in reference {
                        let name = match eval(i.0, ctx)? {
                            Value::Str(s) => s,
                            n => {
                                return Err(Error {
                                    span: expr.1.clone(),
                                    msg: format!("No such variable '{}' in scope", n),
                                })
                            }
                        };

                        if let Some(v) = temp_ctx.local_vars.get(&i.1) {
                            ctx.vars.insert(name.to_string(), v.clone());
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

                    let return_type = ctx.types.get(&f.returns).ok_or_else(|| Error {
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
                Types::Builtin(f) => {
                    if args.len() != f.args.len() {
                        return Err(Error{
                            span: expr.1.clone(),
                            msg: format!("arg count mismatch between definition and call. Expected {} args got {}", f.args.len(), args.len())
                        });
                    }

                    let mut vars_func = Vec::new();

                    for (n, i) in args.iter().enumerate() {
                        // checks if type exsists
                        let t_def = f
                            .args
                            .get(n)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("No arg found at index '{}'", n),
                            })?
                            .clone();
                        let t_type = ctx
                            .types
                            .get(&t_def)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("Type '{}' is not declared", t_def),
                            })?
                            .clone();

                        let v_type = Types::from(eval(i, ctx)?);

                        //checks if types dont match
                        if t_type != v_type {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!(
                                    "Type '{:?}' does not match type '{:?}'",
                                    t_type, v_type
                                ),
                            });
                        }

                        //store the vars on a local var map
                        vars_func.push(eval(i, ctx)?);
                    }

                    match func_string.as_str() {
                        "RIGHT" => Value::Str(vars_func
                            .get(0)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: "Builtin 'Right' needs a string to modify".to_string(),
                            })?
                            .to_string()
                            .chars()
                            .rev()
                            .collect::<String>()
                            .get(0..vars_func.get(1).ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: "Builtin 'Right' needs a number of characters to fetch".to_string(),
                            })?.to_string().parse::<usize>().map_err(|_| Error {
                                span: expr.1.clone(),
                                msg: "Index is not an integer".to_string(),}
                            )?).ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: "Builtin 'Right' needs a string to read".to_string(),
                            })?
                            .to_string()
                            .chars()
                            .rev()
                            .collect::<String>()),

                        "LENGTH" => Value::Int(vars_func
                            .get(0)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: "Builtin 'LENGTH' needs a string to modify".to_string(),
                            })?
                            .to_string()
                            .len()
                            .try_into()
                            .unwrap()),

                        "MID" =>Value::Str(vars_func.get(0).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: "Builtin 'MID' needs a string to modify".to_string(),
                        })?
                        .to_string()[vars_func.get(1)
                        .ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: "Builtin 'MID' needs an index to start reading from".to_string(),
                        })?
                        .to_string()
                        .parse::<usize>()
                        .map_err(|_| Error {
                            span: expr.1.clone(),
                            msg: "Builtin 'MID' Cannot convert starting index to integer".to_string(),
                        })?-1
                        ..
                        vars_func.get(2)
                        .ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: "Builtin 'MID' needs an index to stop reading at".to_string(),
                        })?
                        .to_string()
                        .parse::<usize>()
                        .map_err(|_| Error {
                            span: expr.1.clone(),
                            msg: "Builtin 'MID' Cannot convert ending index to integer".to_string(),
                        })?-1+1]
                        .to_string()),

                        s => return Err(Error {
                            span: expr.1.clone(),
                            msg: format!(
                                "Builtin '{}' has not been defined",
                                s
                            ),
                        })
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
        Expr::Return(v) => Value::Return(Box::new(eval(v, ctx)?)),
        Expr::DeclareArr(name, start, end, type_, then) => {
            let start = match eval(start, ctx)? {
                Value::Int(i) => i,
                v => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("Start of the array must be an integer, found '{}'", v),
                    })
                }
            };
            let end = match eval(end, ctx)? {
                Value::Int(i) => i,
                v => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("End of the array must be an integer, found '{}'", v),
                    })
                }
            };

            ctx.types
                .insert(name.clone(), Types::Array(type_.clone(), start, end));
            ctx.vars.insert(name.clone(), (name.clone(), None));
            let output = eval(then, ctx);
            //types.remove(name);
            //vars.remove(name);
            output?
        }
        Expr::While(cond, body, then) => {
            while eval(cond, ctx)? == Value::Bool(true) {
                match eval(body, ctx)? {
                    Value::Return(t) => return Ok(Value::Return(t)),
                    v => v,
                };
            }

            eval(then, ctx)?
        }
        Expr::Input(var, then) => {
            ctx.channel.send(Actions::Input).unwrap();
            thread::park();

            {
                let string = (*ctx.input.lock().unwrap()).trim().to_string();
                *ctx.input.lock().unwrap() = "".to_string();
                if ctx.vars.get(var).is_some() {
                    ctx.vars.insert(
                        var.clone(),
                        ("STRING".to_string(), Some(Value::Str(string))),
                    );
                } else {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("Var '{}' has not been declared", var),
                    });
                }
            }

            eval(then, ctx)?
        }
        _ => {
            todo!()
        }
    })
}
