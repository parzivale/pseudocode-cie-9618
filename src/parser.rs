use std::{collections::HashMap, fmt::Display};

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
    Array(Vec<Value>),
    Func(String),
    Comp(HashMap<String, Value>),
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

    pub fn is_arr(&self) -> bool {
        match self {
            Value::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_func(&self) -> bool {
        match self {
            Value::Func(_) => true,
            _ => false,
        }
    }

    pub fn is_comp(&self) -> bool {
        match self {
            Value::Comp(_) => true,
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
            Value::Char(s) => write!(f, "{:?}", s),
            _ => write!(f, "display isnt available for this type"),
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
#[derive(Clone, Debug)]
pub enum ArgMode {
    Byref,
    Byval,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Error,
    Value(Value),
    Var(String, Box<Spanned<Self>>),
    DeclarePrim(String, String, Box<Spanned<Self>>),
    DeclareArr(String, Box<Self>, Box<Self>, String, Box<Spanned<Self>>),
    DeclareComp(String, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Assign(Box<(String, Expr)>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Index(
        String,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    Func(String, Vec<(ArgMode, String)>, String, Box<Spanned<Self>>),
    Return(Box<Spanned<Self>>),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    For(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    While(Box<Spanned<Self>>),
    Output(Vec<Spanned<Self>>, Box<Spanned<Self>>),
    Input(String),
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

        let type_ = select! { Token::Type(type_) => type_.clone() }
            .labelled("type")
            .or(ident);

        let declare_prim = just(Token::Keyword("DECLARE".to_string()))
            .ignore_then(ident)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_)
            .labelled("Declare_prim");

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
                    .collect::<Vec<_>>()
                    .map_with_span(|map, span| {
                        let mut new_h = HashMap::new();

                        for i in map.iter() {
                            new_h.insert(i.0.to_string(), Value::Str(i.1.to_owned()));
                        }

                        (Expr::Value(Value::Comp(new_h)), span)
                    })
                    .recover_with(nested_delimiters(
                        Token::Open(Delim::Block),
                        Token::Close(Delim::Block),
                        [],
                        |span| (Expr::Error, span),
                    )),
            )
            .then_ignore(just(Token::Keyword("ENDTYPE".to_string())))
            .then(expr.clone().or_not())
            .map_with_span(|((name, items), body), span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span),
                };
                Expr::DeclareComp(name, Box::new(items), Box::new(body))
            });

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

        let dot_resolve = ident
            .then_ignore(just(Token::Ctrl('.')))
            .then(ident.map(|ident| Expr::Value(Value::Str(ident))));

        let return_ = just(Token::Keyword("RETURN".to_string()))
            .ignore_then(expr.clone())
            .map(|body| Expr::Return(Box::new(body)));

        let assign = dot_resolve
            .clone()
            .or(ident.then(empty().to(Expr::Value(Value::Null))))
            .then_ignore(just(Token::Op("<-".to_string())))
            .then(expr.clone())
            .then(expr.clone().or_not())
            .map_with_span(|((name, v), body), span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span),
                };

                Expr::Assign(Box::new(name), Box::new(v), Box::new(body))
            })
            .labelled("assign");

        let items = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .delimited_by(
                just(Token::Open(Delim::Paren)),
                just(Token::Close(Delim::Paren)),
            );

        let output = just(Token::Keyword("OUTPUT".to_string()))
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .allow_trailing(),
            )
            .then(expr.clone().or_not())
            .map_with_span(|(val, body), span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span),
                };
                Expr::Output(val, Box::new(body))
            });

        let atom = val
            .or(return_)
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
            .or(dot_resolve
                .map_with_span(|(ident, val), span| Expr::Var(ident, Box::new((val, span)))))
            .or(output)
            .or(ident
                .then(empty().to(Expr::Value(Value::Null)))
                .map_with_span(|(ident, val), span| Expr::Var(ident, Box::new((val, span)))))
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

        let raw_expr = compare;

        let block = expr.clone().delimited_by(
            just(Token::Open(Delim::Block)),
            just(Token::Close(Delim::Block)),
        );

        let if_ = recursive(|if_| {
            just(Token::Keyword("IF".to_string()))
                .ignore_then(raw_expr.clone())
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
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .delimited_by(
                just(Token::Open(Delim::Paren)),
                just(Token::Close(Delim::Paren)),
            )
            .labelled("function args");

        let function = just(Token::Keyword("FUNCTION".to_string()))
            .ignore_then(ident)
            .then(args)
            .then_ignore(just(Token::Keyword("RETURNS".to_string())))
            .then(type_)
            .then(block.clone())
            .then_ignore(just(Token::Keyword("ENDFUNCTION".to_string())))
            .map_with_span(|(((name, args), type_), body), span| {
                (Expr::Func(name, args, type_, Box::new(body)), span)
            });

        let procedure = just(Token::Keyword("PROCEDURE".to_string()))
            .ignore_then(ident)
            .then(args.or_not())
            .then(block.clone())
            .then_ignore(just(Token::Keyword("ENDPROCEDURE".to_string())))
            .map_with_span(|((name, args), body), span| {
                (
                    Expr::Func(
                        name,
                        match args {
                            Some(t) => t,
                            None => Vec::new(),
                        },
                        "NULL".to_string(),
                        Box::new(body),
                    ),
                    span,
                )
            });
        let block_expr = if_.or(function).or(procedure).labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| {
                let span = a.1.start..b.1.end;
                (Expr::Then(Box::new(a), Box::new(b)), span)
            });

        let then_block = block_chain.then(expr.clone().or_not()).map(|(a, b)| {
            // This allows creating a span that covers the entire Then expression.
            // b_end is the end of b if it exists, otherwise it is the end of a.
            let a_start = a.1.start;
            let b_end = b.as_ref().map(|b| b.1.end).unwrap_or(a.1.end);
            (
                Expr::Then(
                    Box::new(a),
                    Box::new(match b {
                        Some(b) => b,
                        // Since there is no b expression then its span is empty.
                        None => (Expr::Value(Value::Null), b_end..b_end),
                    }),
                ),
                a_start..b_end,
            )
        });
        then_block.or(raw_expr)
    })
    .then_ignore(end())
}
