use crate::{
    eval::{Error, Types},
    parser::{Expr, Value},
    prelude::*,
};
use std::collections::HashMap;

pub fn comp_fill(
    map: HashMap<String, String>,
    types: &mut HashMap<String, Types>,
    rhs: Value,
    expr: &(Expr, std::ops::Range<usize>),
    last: String,
) -> Result<HashMap<String, (String, Option<Value>)>, Error> {
    let mut temp_types = HashMap::new();
    for i in map.clone() {
        // if types exsists locally then continue local expansion
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
                                    temp.insert(last.clone(), (t.clone(), Some(rhs.clone())));
                                }
                            };
                        }
                        _ => {
                            for i in *s..*e {
                                temp.insert(i.to_string(), (t.clone(), None::<Value>));
                            }
                            temp.insert(last.clone(), (t.clone(), Some(rhs.clone())));
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
