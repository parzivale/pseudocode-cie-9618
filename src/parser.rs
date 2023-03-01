use std::{collections::HashMap, fmt::Display, ops::Range};

use crate::{
    eval::Types,
    lexer::{Delim, Token},
    prelude::*,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i32),
    Real(f32),
    Str(String),
    Char(char),
    Comp(HashMap<String, (String, Option<Value>)>),
    Func(String),
    Return(Box<Self>),
}

impl Value {
    pub fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Value::Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Value::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Value::Real(_) => true,
            _ => false,
        }
    }
    pub fn is_str(&self) -> bool {
        match self {
            Value::Str(_) => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            Value::Char(_) => true,
            _ => false,
        }
    }
    pub fn is_comp(&self) -> bool {
        match self {
            Value::Comp(_) => true,
            _ => false,
        }
    }

    pub fn is_func(&self) -> bool {
        match self {
            Value::Func(_) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "Null"),
            Value::Int(s) => write!(f, "{:?}", s),
            Value::Real(s) => write!(f, "{:?}", s),
            Value::Bool(s) => write!(f, "{:?}", s),
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Char(s) => write!(f, "{:?}", s),
            Value::Func(s) => write!(f, "{:?}", s),
            _ => write!(f, "display isnt available for this type"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Le,
    Leq,
    Ge,
    Geq,
}
#[derive(Clone, Debug, PartialEq)]
pub enum ArgMode {
    Byref,
    Byval,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Error,
    Name(Box<Spanned<Self>>),
    Value(Value),
    CompVar(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Var(Box<Spanned<Self>>),
    DeclarePrim(String, String, Box<Spanned<Self>>),
    DeclareArr(
        String,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        String,
        Box<Spanned<Self>>,
    ),
    DeclareComp(String, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Assign(
        Box<Spanned<Self>>,
        Vec<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    Func(
        String,
        Vec<((ArgMode, String), String)>,
        String,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    Return(Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    ProcCall(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Call(String, Vec<Spanned<Self>>),
    If(
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    For(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    While(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Output(Vec<Spanned<Self>>, Box<Spanned<Self>>),
    Input(String, Box<Spanned<Self>>),
}

pub fn parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let val = select! {
            Token::Bool(x) => Expr::Value(Value::Bool(x)),
            Token::Num(n) => {
                let res = n.parse::<i32>();
                if res.is_ok() {
                    Expr::Value(Value::Int(res.unwrap()))
                } else {
                    let res = n.parse::<f32>();
                    if res.is_ok() {
                        Expr::Value(Value::Real(res.unwrap()))
                    } else {
                        Expr::Error
                    }
                }
            },
            Token::Str(s) => Expr::Value(Value::Str(s)),
            Token::Char(s) => Expr::Value(Value::Char(s)),
        }
        .labelled("value");

        let ident = select! {Token::Ident(item) => item.clone()}.labelled("indentifier");

        let return_ = just(Token::Keyword("RETURN".to_string())).ignore_then(expr.clone());

        let type_ = select! { Token::Type(type_) => type_.clone() }
            .labelled("type")
            .or(ident);

        let items = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .delimited_by(
                just(Token::Open(Delim::Paren)),
                just(Token::Close(Delim::Paren)),
            );

        let call_function = ident
            .then(items.clone())
            .labelled("call")
            .map(|(f, args)| Expr::Call(f, args));
        let base_expr = recursive(|base_expr| {
            let atom = val
                .or(return_.map(|a| Expr::Return(Box::new(a))))
                .or(call_function)
                .or(ident.map_with_span(|ident, span: Span| {
                    Expr::Var(Box::new((Expr::Value(Value::Str(ident)), span.clone())))
                }))
                .labelled("atom")
                .map_with_span(|expr, span| (expr, span))
                .or(expr.clone().delimited_by(
                    just(Token::Open(Delim::Paren)),
                    just(Token::Close(Delim::Paren)),
                ));
            let op = just(Token::Ctrl('.')).labelled("Comp_access");

            let dot = atom
                .clone()
                .then(op.ignore_then(atom.clone()).repeated())
                .foldl(|a, b| {
                    let span = a.1.start..b.1.end;

                    (
                        Expr::CompVar(
                            Box::new(a),
                            Box::new(match b.0.clone() {
                                Expr::Value(t) => {
                                    (Expr::Var(Box::new((Expr::Value(t), b.1.clone()))), b.1)
                                }
                                Expr::Binary(lhs, op, rhs) => (
                                    Expr::Var(Box::new((Expr::Binary(lhs, op, rhs), b.1.clone()))),
                                    b.1,
                                ),
                                _ => b,
                            }),
                        ),
                        span,
                    )
                });

            let index = dot
                .clone()
                .then(
                    base_expr
                        .clone()
                        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                        .repeated(),
                )
                .foldl(|a, b: (Expr, Range<usize>)| {
                    let span = a.1.start..b.1.end;

                    (
                        Expr::CompVar(
                            Box::new(a),
                            Box::new(match b.0.clone() {
                                Expr::Value(t) => {
                                    (Expr::Var(Box::new((Expr::Value(t), b.1.clone()))), b.1)
                                }
                                Expr::Binary(lhs, op, rhs) => (
                                    Expr::Var(Box::new((Expr::Binary(lhs, op, rhs), b.1.clone()))),
                                    b.1,
                                ),
                                _ => b,
                            }),
                        ),
                        span,
                    )
                });
            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div))
                .labelled("binop_mult");
            let product =
                index
                    .clone()
                    .then(op.then(index.clone()).repeated())
                    .foldl(|a, (op, b)| {
                        let span = a.1.start..b.1.end;
                        (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                    });

            let op = just(Token::Op("+".to_string()))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub))
                .labelled("binop_addition");
            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });
            sum
        });

        let op = just(Token::Op("=".to_string()))
            .to(BinaryOp::Eq)
            .or(just(Token::Op("<>".to_string())).to(BinaryOp::NotEq))
            .or(just(Token::Op(">".to_string())).to(BinaryOp::Ge))
            .or(just(Token::Op("<".to_string())).to(BinaryOp::Le))
            .or(just(Token::Op(">=".to_string())).to(BinaryOp::Geq))
            .or(just(Token::Op("<=".to_string())).to(BinaryOp::Leq))
            .labelled("binop_comp");

        let compare = base_expr
            .clone()
            .then(op.then(base_expr).repeated())
            .labelled("comp")
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            });

        let raw_expr = compare;

        let block = expr.clone().delimited_by(
            just(Token::Open(Delim::Block)),
            just(Token::Close(Delim::Block)),
        );

        let if_ = recursive(|if_| {
            just(Token::Keyword("IF".to_string()))
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Open(Delim::Block)))
                .then_ignore(just(Token::Keyword("THEN".to_string())))
                .then(block.clone())
                .then(
                    just(Token::Keyword("ELSE".to_string()))
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .then_ignore(just(Token::Close(Delim::Block)))
                .then_ignore(just(Token::Keyword("ENDIF".to_string())))
                .then(expr.clone().or_not())
                .map_with_span(|(((cond, a), b), body), span: Span| {
                    (
                        Expr::If(
                            Box::new(cond),
                            Box::new(a),
                            Box::new(match b {
                                Some(b) => b,
                                // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                            Box::new(match body {
                                Some(t) => t,
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                        ),
                        span,
                    )
                })
        });

        let while_ = recursive(|while_| {
            just(Token::Keyword("WHILE".to_string()))
                .ignore_then(expr.clone())
                .then(block.clone().or(while_))
                .then_ignore(just(Token::Keyword("ENDWHILE".to_string())))
                .then(expr.clone().or_not())
                .map_with_span(|((cond, body), then), span: Span| {
                    (
                        Expr::While(
                            Box::new(cond),
                            Box::new(body),
                            Box::new(match then {
                                Some(t) => t,
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                        ),
                        span.clone(),
                    )
                })
        });

        let args = just(Token::Keyword("BYVAL".to_string()))
            .or(just(Token::Keyword("BYREF".to_string())))
            .or_not()
            .map(|t| match t {
                Some(t) => match t {
                    Token::Keyword(s) => match s.as_str() {
                        "BYVAL" => ArgMode::Byval,
                        "BYREF" => ArgMode::Byref,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                },
                None => ArgMode::Byval,
            })
            .then(ident.clone())
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_.clone())
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .delimited_by(
                just(Token::Open(Delim::Paren)),
                just(Token::Close(Delim::Paren)),
            )
            .labelled("function args");

        let call_procedure = just(Token::Keyword("CALL".to_string()))
            .ignore_then(ident)
            .then(items.clone().or_not())
            .then(expr.clone().or_not())
            .labelled("call_procedure")
            .map_with_span(|((f, args), body), span: Span| {
                (
                    Expr::ProcCall(
                        Box::new((
                            Expr::Call(
                                f,
                                match args {
                                    Some(args) => args,
                                    None => Vec::new(),
                                },
                            ),
                            span.clone(),
                        )),
                        Box::new(match body {
                            Some(t) => t,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span.clone(),
                )
            });
        let function = just(Token::Keyword("FUNCTION".to_string()))
            .ignore_then(ident)
            .then(args.clone().or_not())
            .then_ignore(just(Token::Keyword("RETURNS".to_string())))
            .then(type_)
            .then(expr.clone().delimited_by(
                just(Token::Open(Delim::Block)),
                just(Token::Close(Delim::Block)),
            ))
            .then_ignore(just(Token::Keyword("ENDFUNCTION".to_string())))
            .then(expr.clone().or_not())
            .map_with_span(|((((name, args), type_), body), then), span: Span| {
                (
                    Expr::Func(
                        name,
                        match args {
                            Some(t) => t,
                            None => Vec::new(),
                        },
                        type_,
                        Box::new(body),
                        Box::new(match then {
                            Some(t) => t,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            });

        let procedure = just(Token::Keyword("PROCEDURE".to_string()))
            .ignore_then(ident)
            .then(args.clone().or_not())
            .then(block.clone())
            .then_ignore(just(Token::Keyword("ENDPROCEDURE".to_string())))
            .then(expr.clone())
            .map_with_span(|(((name, args), body), then), span| {
                (
                    Expr::Func(
                        name,
                        match args {
                            Some(t) => t,
                            None => Vec::new(),
                        },
                        "NULL".to_string(),
                        Box::new(body),
                        Box::new(then),
                    ),
                    span,
                )
            });
        let declare_prim = just(Token::Keyword("DECLARE".to_string()))
            .ignore_then(ident)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_)
            .then(expr.clone().or_not())
            .map_with_span(|((name, type_), then), span: Span| {
                (
                    Expr::DeclarePrim(
                        name,
                        type_,
                        match then {
                            Some(t) => Box::new(t),
                            None => Box::new((Expr::Value(Value::Null), span.clone())),
                        },
                    ),
                    span.clone(),
                )
            })
            .labelled("Declare_prim");

        let declare_arr = just(Token::Keyword("DECLARE".to_string()))
            .ignore_then(ident)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_)
            .then_ignore(just(Token::Ctrl('[')))
            .then(expr.clone())
            .then_ignore(just(Token::Ctrl(':')))
            .then(expr.clone())
            .then_ignore(just(Token::Ctrl(']')))
            .then_ignore(just(Token::Keyword("OF".to_string())))
            .then(type_)
            .then(expr.clone().or_not())
            .map_with_span(|(((((name, _), start), end), type_), then), span: Span| {
                (
                    Expr::DeclareArr(
                        name,
                        Box::new(start),
                        Box::new(end),
                        type_,
                        Box::new(match then {
                            Some(t) => t,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span.clone(),
                )
            })
            .labelled("Declare_arr");

        let assign = ident
            .clone()
            .then(
                just(Token::Ctrl('.'))
                    .ignore_then(
                        ident
                            .clone()
                            .map_with_span(|ident, span| (Expr::Value(Value::Str(ident)), span)),
                    )
                    .or(raw_expr
                        .clone()
                        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']'))))
                    .repeated(),
            )
            .then_ignore(just(Token::Op("<-".to_string())))
            .then(expr.clone())
            .then(expr.clone().or_not())
            .map_with_span(|(((name, children), v), body), span: Span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span.clone()),
                };

                (
                    Expr::Assign(
                        Box::new((Expr::Value(Value::Str(name)), span.clone())),
                        children,
                        Box::new(v),
                        Box::new(body),
                    ),
                    span.clone(),
                )
            })
            .labelled("assign");

        let output = just(Token::Keyword("OUTPUT".to_string()))
            .ignore_then(expr.clone().separated_by(just(Token::Ctrl(','))))
            .then(expr.clone().or_not())
            .map_with_span(|(val, body), span: Span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span.clone()),
                };
                (Expr::Output(val, Box::new(body)), span.clone())
            });

        let input = just(Token::Keyword("INPUT".to_string()))
            .ignore_then(ident.clone())
            .then(expr.clone().or_not())
            .map_with_span(|(val, body), span: Span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span.clone()),
                };
                (Expr::Input(val, Box::new(body)), span.clone())
            });

        let declare_comp = just(Token::Keyword("TYPE".to_string()))
            .ignore_then(ident.clone())
            .then(declare_arr.clone().or(declare_prim.clone()).delimited_by(
                just(Token::Open(Delim::Block)),
                just(Token::Close(Delim::Block)),
            ))
            .then_ignore(just(Token::Keyword("ENDTYPE".to_string())))
            .then(expr.clone().or_not())
            .map_with_span(|((name, items), body), span: Span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span.clone()),
                };
                (
                    Expr::DeclareComp(name, Box::new(items), Box::new(body)),
                    span.clone(),
                )
            });

        if_.or(function)
            .or(procedure)
            .or(call_procedure)
            .or(while_)
            .or(assign)
            .or(output)
            .or(input)
            .or(declare_comp)
            .or(declare_arr)
            .or(declare_prim)
            .or(raw_expr)
            .labelled("block")
    })
    .then_ignore(end())
}
