use crate::{
    eval::{eval, Ctx, Error},
    parser::{Expr, Value},
    prelude::*,
};

pub fn bin_add(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Int(a + b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot add {} to {} due to confilcting types", a, b),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Real(a + b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot add {} to {} due to confilcting types", a, b),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_mul(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Int(a * b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("cannot mutliply {} to {} due to confilcting types", a, b),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Real(a * b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("cannot mutliply {} to {} due to confilcting types", a, b),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_sub(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Int(a - b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("cannot subtract {} from {} due to confilcting types", b, a),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Real(a - b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("cannot subtract {} from {} due to confilcting types", b, a),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_div(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Real(a as f32 / b as f32))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("cannot divide {} dy {} due to confilcting types", b, a),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Real(a / b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("cannot divide {} by {} due to confilcting types", b, a),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_ge(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Bool(a > b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Bool(a > b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_geq(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Bool(a >= b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Bool(a >= b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_le(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Bool(a < b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Bool(a < b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_leq(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Int(a) => {
            if let Value::Int(b) = b {
                Ok(Value::Bool(a <= b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        Value::Real(a) => {
            if let Value::Real(b) = b {
                Ok(Value::Bool(a <= b))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot compare '{}' to '{}'", b, a),
                })
            }
        }
        _ => unreachable!(),
    }
}

pub fn bin_concat(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    ctx: &mut Ctx,
    expr: &Spanned<Expr>,
) -> Result<Value, Error> {
    let a = eval(a, ctx)?;
    let b = eval(b, ctx)?;

    match a {
        Value::Str(mut a) => {
            if let Value::Str(b) = b {
                a.push_str(b.as_str());
                Ok(Value::Str(a))
            } else {
                Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot concatenate '{}' to '{}'", b, a),
                })
            }
        }
        _ => unreachable!(),
    }
}
