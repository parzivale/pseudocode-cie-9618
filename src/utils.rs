use crate::{
    eval::{eval, Ctx, Error, Types},
    parser::{Expr, Value},
    prelude::*,
};
use std::{collections::HashMap, sync::Arc};

pub fn comp_fill(
    map: HashMap<String, String>,
    types: &mut HashMap<String, Types>,
    rhs: Value,
    expr: &Spanned<Expr>,
    last: String,
) -> Result<HashMap<String, (String, Option<Value>)>, Error> {
    let mut temp_types = HashMap::new();
    for i in map {
        // if types exsists locally then continue local expansion
        match types.get(&i.1) {
            Some(t) => match t {
                // if its a composite fill its children
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
                            Some(Types::Array(_, _, _)) => {
                                temp_types.insert(
                                    i.0.clone(),
                                    (
                                        i.1.clone(),
                                        Some(Value::Arr(comp_fill(
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
                // also fill arrays
                Types::Array(t, s, e) => {
                    let mut temp = HashMap::new();
                    match types.get(t) {
                        Some(Types::Composite(h)) => {
                            for i in *s..(*e + 1) {
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
                            // needed extra levels as we dont get the hash map of types of the array
                            // from the type
                            match types.get(t) {
                                Some(Types::Composite(h)) => {
                                    for i in *s..(*e + 1) {
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
                                    for i in *s..(*e + 1) {
                                        h.insert(i.to_string(), t.clone());
                                    }

                                    for i in *s..(*e + 1) {
                                        temp.insert(
                                            i.to_string(),
                                            (
                                                t.clone(),
                                                Some(Value::Arr(comp_fill(
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
                                    for i in *s..(*e + 1) {
                                        temp.insert(i.to_string(), (t.clone(), None::<Value>));
                                    }
                                    temp.insert(last.clone(), (t.clone(), Some(rhs.clone())));
                                }
                            };
                        }
                        // not composite we can just fill with whatever
                        _ => {
                            for i in *s..(*e + 1) {
                                temp.insert(i.to_string(), (t.clone(), None::<Value>));
                            }
                            temp.insert(last.clone(), (t.clone(), Some(rhs.clone())));
                        }
                    }
                    temp_types.insert(i.0.clone(), (i.1.clone(), Some(Value::Arr(temp))));
                }
                // if its not a composite type just fill with whatever
                _ => {
                    if i.0.clone() == last {
                        temp_types.insert(i.0.clone(), (i.1.clone(), Some(rhs.clone())));
                    } else {
                        temp_types.insert(i.0.clone(), (i.1.clone(), None));
                    }
                }
            },
            t => {
                return Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Unexpected type '{:?}'", t),
                })
            }
        };
    }

    Ok(temp_types)
}

pub fn to_type_map(
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

#[allow(clippy::too_many_arguments)]
pub fn update_comp_vars(
    current_var: (String, Option<Value>),
    children: &Vec<Spanned<Expr>>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
    temp_types: HashMap<String, String>,
    rhs: Value,
    name: String,
    h: HashMap<String, String>,
) -> Result<(), Error> {
    let (type_str_, _) = ctx
        .vars
        .get(&name)
        .ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("'{}' Has not been declared", name),
        })?
        .clone();

    let mut maps = Vec::new();
    if let Some(mut current_var) = current_var.1 {
        for i in children {
            let i = eval(i, ctx)?;
            match i {
                Value::Str(s) => match current_var.clone() {
                    Value::Comp(h) => {
                        maps.push(current_var);
                        current_var = match h
                            .get(&s)
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("Couldn't find key '{}'", s),
                            })?
                            .clone()
                            .1
                        {
                            Some(t) => t,
                            _ => break,
                        };
                    }
                    v => maps.push(v),
                },
                Value::Int(i) => match current_var.clone() {
                    // check if child is a array
                    Value::Arr(h) => {
                        maps.push(current_var);
                        current_var = match h
                            .get(&i.to_string())
                            .ok_or_else(|| Error {
                                span: expr.1.clone(),
                                msg: format!("Couldn't find key '{}'", i),
                            })?
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
        let comb = maps.iter().rev().zip(children.iter().rev());
        for (i, index) in comb {
            let mut temp_ctx = Ctx {
                vars: ctx.vars.clone(),
                types: to_type_map(temp_types.clone(), &mut ctx.types, expr)?,
                local_vars: ctx.local_vars.clone(),
                channel: ctx.channel.clone(),
                input: Arc::clone(&ctx.input),
                file_read: Arc::clone(&ctx.file_read),
                open_files: HashMap::new(),
            };
            let index = eval(index, &mut temp_ctx)?;
            match index {
                Value::Str(s) => {
                    match i {
                        Value::Comp(h) => {
                            let type_ = h
                                .get(&s)
                                .ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!("Couldn't find key '{}'", s),
                                })?
                                .clone()
                                .0;
                            let mut h = h.clone();
                            h.insert(s, (type_, Some(map)));
                            map = Value::Comp(h);
                        }
                        Value::Arr(h) => {
                            let type_ = h
                                .get(&s)
                                .ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!("Couldn't find key '{}'", s),
                                })?
                                .clone()
                                .0;
                            let mut h = h.clone();
                            h.insert(s, (type_, Some(map)));
                            map = Value::Arr(h);
                        }
                        v => {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!(
                                    "Cannot update composite component of a primitive  '{}'",
                                    v
                                ),
                            })
                        }
                    };
                }
                Value::Int(i_) => {
                    match i {
                        Value::Comp(h) => {
                            let type_ = h
                                .get(&i_.to_string())
                                .ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!("Couldn't find key '{}'", i_),
                                })?
                                .clone()
                                .0;
                            let mut h = h.clone();
                            h.insert(i_.to_string(), (type_, Some(map)));
                            map = Value::Comp(h);
                        }
                        Value::Arr(h) => {
                            let type_ = h
                                .get(&i_.to_string())
                                .ok_or_else(|| Error {
                                    span: expr.1.clone(),
                                    msg: format!("Couldn't find key '{}'", i_),
                                })?
                                .clone()
                                .0;
                            let mut h = h.clone();
                            h.insert(i_.to_string(), (type_, Some(map)));
                            map = Value::Arr(h);
                        }
                        v => {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!("Unexpected value '{}'", v),
                            })
                        }
                    };
                }
                v => {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!("Unexpected value '{}'", v),
                    })
                }
            };
        }

        ctx.vars.insert(name.clone(), (type_str_, Some(map)));

    // if the variable is not assigned a value we can just create a new value
    // this section might be more apt in declare prim but for now this is
    // the best place for it
    } else if let Value::Comp(h) = rhs {
        let type_ = ctx.types.get(&type_str_).ok_or(Error {
            span: expr.1.clone(),
            msg: "Cannot find type associated with composite type (Create empty composite)"
                .to_string(),
        })?;
        match type_ {
            Types::Composite(_) => Ok(ctx
                .vars
                .insert(name.clone(), (type_str_, Some(Value::Comp(h))))),
            Types::Array(_, _, _) => Ok(ctx
                .vars
                .insert(name.clone(), (type_str_, Some(Value::Arr(h))))),
            _ => Err(Error {
                span: expr.1.clone(),
                msg: "Got non composite type (Create empty composite)".to_string(),
            }),
        }?;
    } else {
        let last = children.last().ok_or_else(|| Error {
            span: expr.1.clone(),
            msg: format!("No values to access composite type '{}'", name),
        })?;
        let last = match eval(last, ctx)? {
            Value::Str(s) => s,
            Value::Int(i) => i.to_string(),
            v => {
                return Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Unexpected value '{}'", v),
                })
            }
        };

        let type_ = ctx.types.get(&type_str_).unwrap();
        match type_ {
            Types::Composite(_) => ctx.vars.insert(
                name.clone(),
                (
                    type_str_,
                    Some(Value::Comp(comp_fill(
                        h,
                        &mut ctx.types.clone(),
                        rhs,
                        &expr.clone(),
                        last,
                    )?)),
                ),
            ),
            Types::Array(_, _, _) => ctx.vars.insert(
                name.clone(),
                (
                    type_str_,
                    Some(Value::Arr(comp_fill(
                        h,
                        &mut ctx.types.clone(),
                        rhs,
                        &expr.clone(),
                        last,
                    )?)),
                ),
            ),
            _ => panic!(),
        };
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub fn type_check_comp(
    children: &Vec<Spanned<Expr>>,
    ctx: &mut Ctx,
    mut temp_types: HashMap<String, String>,
    expr: &Spanned<Expr>,
    mut final_type: Types,
    rhs: &Value,
    name: &String,
) -> Result<HashMap<String, String>, Error> {
    for i in children {
        let mut temp_ctx = Ctx {
            vars: ctx.vars.clone(),
            local_vars: ctx.local_vars.clone(),
            types: to_type_map(temp_types.clone(), &mut ctx.types, expr)?,
            channel: ctx.channel.clone(),
            input: Arc::clone(&ctx.input),
            file_read: Arc::clone(&ctx.file_read),
            open_files: HashMap::new(),
        };

        let i = eval(i, &mut temp_ctx)?;

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

                match ctx.types.get(child_type) {
                    Some(Types::Composite(h)) => {
                        final_type = Types::Composite(h.clone());
                        temp_types = h.clone()
                    }
                    Some(Types::Array(t, _, _)) => {
                        let type_ = ctx.types.get(t).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: format!("Type '{}' has not been declared", t),
                        })?;
                        final_type = type_.clone();
                        if let Types::Composite(h) = type_ {
                            temp_types = h.clone();
                        }
                    }
                    Some(t) => {
                        final_type = t.clone();
                    }
                    t => {
                        return Err(Error {
                            span: expr.1.clone(),
                            msg: format!("Unexpected type '{:?}'", t),
                        })
                    }
                };
            }
            //we dont need to handle array indexing here, we only care about the type
            Value::Int(i) => {
                let s = i.to_string();
                let child_type = temp_types.get(&s).ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Child '{}' does not exsist on composite type '{:?}'",
                        s, temp_types
                    ),
                })?;

                match ctx.types.get(child_type) {
                    Some(Types::Composite(h)) => {
                        final_type = Types::Composite(h.clone());
                        temp_types = h.clone()
                    }
                    Some(Types::Array(t, _, _)) => {
                        let type_ = ctx.types.get(t).ok_or_else(|| Error {
                            span: expr.1.clone(),
                            msg: format!("Type '{}' has not been declared", t),
                        })?;
                        final_type = type_.clone();
                        if let Types::Composite(h) = type_ {
                            temp_types = h.clone();
                        }
                    }
                    Some(t) => {
                        final_type = t.clone();
                    }
                    t => {
                        if let Some(t) = t {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!("Unexpected type '{:?}'", t),
                            });
                        } else {
                            return Err(Error {
                                span: expr.1.clone(),
                                msg: format!("Expected type got '{:?}'", t),
                            });
                        }
                    }
                };
            }
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
            msg: format!("'{}' seems to have a NULL type, which is impossible", name),
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
    Ok(temp_types)
}
