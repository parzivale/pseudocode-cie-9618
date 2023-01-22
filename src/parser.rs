use std::{collections::HashMap, fmt::Display};

use crate::{
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
    Array(Vec<Value>),
    Func(String),
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
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Array(s) => write!(f, "{:?}", s),
            Value::Null => write!(f, "Null"),
            Value::Int(s) => write!(f, "{:?}", s),
            Value::Real(s) => write!(f, "{:?}", s),
            Value::Bool(s) => write!(f, "{:?}", s),
            Value::Func(s) => write!(f, "{:?}", s),
            Value::Str(s) => write!(f, "{:?}", s),
        }
    }
}

// A function node in the AST.
#[derive(Debug)]
pub struct Func {
    args: Vec<String>,
    body: Spanned<Expr>,
}

#[derive(Clone, Debug)]
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

#[derive(Debug)]
pub enum Expr {
    Error,
    Value(Value),
    Var(String),
    Array(Vec<Spanned<Self>>),
    DeclarePrim(String, String, Box<Spanned<Self>>),
    DeclareArr(String, Box<Self>, Box<Self>, String, Box<Spanned<Self>>),
    DeclareComp(String, Vec<(String, String)>, Box<Spanned<Self>>),
    Assign(String, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Index(
        String,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    For(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    While(Box<Spanned<Self>>),
    Repeat(Box<Spanned<Self>>),
    Output(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Input(String),
}

pub fn parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expr = recursive(|raw_expr| {
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
            }
            .labelled("value");

            let ident = select! {Token::Ident(item) => item.clone()}.labelled("indentifier");

            let type_ = select! { Token::Type(type_) => type_.clone() }.labelled("type");

            let declare_prim = just(Token::Keyword("DECLARE".to_string()))
                .ignore_then(ident)
                .then_ignore(just(Token::Ctrl(':')))
                .then(type_)
                .labelled("Declare_prim");

            let declare_arr = declare_prim
                .clone()
                .then_ignore(just(Token::Ctrl('[')))
                .then(val.clone())
                .then_ignore(just(Token::Ctrl(':')))
                .then(val.clone())
                .then_ignore(just(Token::Ctrl(']')))
                .then_ignore(just(Token::Keyword("OF".to_string())))
                .then(type_)
                .then(expr.clone().or_not())
                .map_with_span(|((((name, val1), val2), t), body), span| {
                    let body = match body {
                        Some(t) => t,
                        None => (Expr::Value(Value::Null), span),
                    };
                    Expr::DeclareArr(name.0, Box::new(val1), Box::new(val2), t, Box::new(body))
                })
                .labelled("Declare_arr");

            let declare_comp = just(Token::Keyword("TYPE".to_string()))
                .ignore_then(ident.clone())
                .then(
                    declare_prim
                        .clone()
                        .repeated()
                        .delimited_by(
                            just(Token::Open(Delim::Block)),
                            just(Token::Close(Delim::Block)),
                        )
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Keyword("ENDTYPE".to_string())))
                .then(expr.clone().or_not())
                .map_with_span(|((name, items), body), span| {
                    let body = match body {
                        Some(t) => t,
                        None => (Expr::Value(Value::Null), span),
                    };
                    Expr::DeclareComp(name, items, Box::new(body))
                });

            let assign = ident
                .clone()
                .then_ignore(just(Token::Op("<-".to_string())))
                .then(raw_expr.clone())
                .then(expr.clone().or_not())
                .map_with_span(|((name, v), body), span| {
                    let body = match body {
                        Some(t) => t,
                        None => (Expr::Value(Value::Null), span),
                    };
                    Expr::Assign(name, Box::new(v), Box::new(body))
                })
                .labelled("assign");

            let index = ident
                .clone()
                .then(
                    raw_expr
                        .clone()
                        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']'))),
                )
                .then_ignore(just(Token::Op("<-".to_string())))
                .then(raw_expr.clone())
                .then(expr.clone().or_not())
                .map_with_span(|(((name, v1), v2), body), span| {
                    let body = match body {
                        Some(t) => t,
                        None => (Expr::Value(Value::Null), span),
                    };
                    Expr::Index(name, Box::new(v1), Box::new(v2), Box::new(body))
                });

            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing();

            let output = just(Token::Keyword("OUTPUT".to_string()))
                .ignore_then(raw_expr.clone())
                .then(expr.clone().or_not())
                .map_with_span(|(val, body), span| {
                    let body = match body {
                        Some(t) => t,
                        None => (Expr::Value(Value::Null), span),
                    };
                    Expr::Output(Box::new(val), Box::new(body))
                });

            let atom = val
                .or(declare_arr)
                .or(declare_comp)
                .or(declare_prim.then(expr.clone().or_not()).map_with_span(
                    |((name, t), body), span| {
                        let body = match body {
                            Some(t) => t,
                            None => (Expr::Value(Value::Null), span),
                        };
                        Expr::DeclarePrim(name, t, Box::new(body))
                    },
                ))
                .or(assign)
                .or(index)
                .or(output)
                .or(ident.map(Expr::Var))
                .labelled("atom")
                .map_with_span(|expr, span| (expr, span))
                .or(expr.clone().delimited_by(
                    just(Token::Open(Delim::Paren)),
                    just(Token::Close(Delim::Paren)),
                ));

            let call_keyword = atom
                .clone()
                .or(just(Token::Keyword("CALL".to_string())).ignore_then(atom.clone()))
                .labelled("call_kw");

            let call = call_keyword
                .then(
                    items
                        .delimited_by(
                            just(Token::Open(Delim::Paren)),
                            just(Token::Close(Delim::Paren)),
                        )
                        .map_with_span(|args, span: Span| (args, span))
                        .repeated(),
                )
                .labelled("call")
                .foldl(|f, args| {
                    let span = f.1.start..args.1.end;
                    (Expr::Call(Box::new(f), args.0), span)
                });

            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div))
                .labelled("binop_mult");
            let product = call
                .clone()
                .then(op.then(call).repeated())
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

            let op = just(Token::Op("=".to_string()))
                .to(BinaryOp::Eq)
                .or(just(Token::Op("<>".to_string())).to(BinaryOp::NotEq))
                .or(just(Token::Op(">".to_string())).to(BinaryOp::Ge))
                .or(just(Token::Op("<".to_string())).to(BinaryOp::Le))
                .or(just(Token::Op(">=".to_string())).to(BinaryOp::Geq))
                .or(just(Token::Op("<=".to_string())).to(BinaryOp::Leq))
                .labelled("binop_comp");

            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .labelled("comp")
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });
            compare
        });

        let block = expr.clone().delimited_by(
            just(Token::Open(Delim::Block)),
            just(Token::Close(Delim::Block)),
        );
        // Attempt to recover anything that looks like a block but contains errors

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
                .map_with_span(|((cond, a), b), span: Span| {
                    (
                        Expr::If(
                            Box::new(cond),
                            Box::new(a),
                            Box::new(match b {
                                Some(b) => b,
                                // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                        ),
                        span,
                    )
                })
        });

        let block_expr = if_.labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| {
                let span = a.1.start..b.1.end;
                (Expr::Then(Box::new(a), Box::new(b)), span)
            });

        block_chain.or(raw_expr.clone())
    })
    .then_ignore(end())
}
