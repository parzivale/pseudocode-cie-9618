use std::{
    fmt::Display,
    io::{BufReader, BufWriter},
    ops::Range,
};

use crate::{
    eval::VarMap,
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
    Comp(VarMap),
    Arr(VarMap),
    Func(String),
    Builtin(String),
    Return(Box<Self>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "Null"),
            Value::Int(s) => write!(f, "{}", s),
            Value::Real(s) => write!(f, "{}", s),
            Value::Bool(s) => write!(f, "{}", s),
            Value::Str(s) => write!(f, "{}", s),
            Value::Char(s) => write!(f, "{}", s),
            Value::Func(s) => write!(f, "{}", s),
            _ => write!(f, "display isnt available for this type"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Concat,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgMode {
    Byref,
    Byval,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FileMode {
    Read,
    Write,
    Append,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Error,
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
    For(
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    While(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Output(Vec<Spanned<Self>>, Box<Spanned<Self>>),
    Input(Box<Spanned<Self>>, Vec<Spanned<Self>>, Box<Spanned<Self>>),
    OpenFile(Box<Spanned<Self>>, FileMode, Box<Spanned<Self>>),
    CloseFile(Box<Spanned<Self>>, Box<Spanned<Self>>),
    WriteFile(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    ReadFile(
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Vec<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
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

        let ident = select! {Token::Ident(item) => item}.labelled("indentifier");

        let type_ = select! { Token::Type(type_) => type_}
            .labelled("type")
            .or(ident);

        let base_expr = recursive(|base_expr| {
            let items = base_expr
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
            let atom = val
                .or(call_function)
                .or(ident.map_with_span(|ident, span: Span| {
                    Expr::Var(Box::new((Expr::Value(Value::Str(ident)), span)))
                }))
                .labelled("atom")
                .map_with_span(|expr, span| (expr, span))
                .or(base_expr.clone().delimited_by(
                    just(Token::Open(Delim::Paren)),
                    just(Token::Close(Delim::Paren)),
                ));
            let op = just(Token::Ctrl('.')).labelled("Comp_access");

            let index = atom
                .clone()
                .then(
                    base_expr
                        .clone()
                        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                        .or(op.ignore_then(atom.clone()))
                        .repeated(),
                )
                .foldl(|a: (Expr, Range<usize>), b: (Expr, Range<usize>)| {
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
            let op = just(Token::Op("&".to_string()))
                .to(BinaryOp::Concat)
                .labelled("binop_mult");
            let concat = index.clone().then(op.then(index.clone()).repeated()).foldl(
                |a: (Expr, Range<usize>), (op, b): (BinaryOp, (Expr, Range<usize>))| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                },
            );

            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div))
                .labelled("binop_mult");
            let product = concat
                .clone()
                .then(op.then(concat.clone()).repeated())
                .foldl(
                    |a: (Expr, Range<usize>), (op, b): (BinaryOp, (Expr, Range<usize>))| {
                        let span = a.1.start..b.1.end;
                        (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                    },
                );

            let op = just(Token::Op("+".to_string()))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub))
                .labelled("binop_addition");
            product.clone().then(op.then(product).repeated()).foldl(
                |a: (Expr, Range<usize>), (op, b): (BinaryOp, (Expr, Range<usize>))| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                },
            )
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

        let items = raw_expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .delimited_by(
                just(Token::Open(Delim::Paren)),
                just(Token::Close(Delim::Paren)),
            );

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
                .ignore_then(raw_expr.clone())
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
            .then(ident)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_)
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
                    span,
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
                    span,
                )
            })
            .labelled("Declare_prim");

        let declare_arr = just(Token::Keyword("DECLARE".to_string()))
            .ignore_then(ident)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_)
            .then_ignore(just(Token::Ctrl('[')))
            .then(raw_expr.clone())
            .then_ignore(just(Token::Ctrl(':')))
            .then(raw_expr.clone())
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
                    span,
                )
            })
            .labelled("Declare_arr");

        let assign = ident
            .then(
                just(Token::Ctrl('.'))
                    .ignore_then(
                        ident.map_with_span(|ident, span| (Expr::Value(Value::Str(ident)), span)),
                    )
                    .or(raw_expr
                        .clone()
                        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']'))))
                    .repeated(),
            )
            .then_ignore(just(Token::Op("<-".to_string())))
            .then(raw_expr.clone())
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
                    span,
                )
            })
            .labelled("assign");

        let output = just(Token::Keyword("OUTPUT".to_string()))
            .ignore_then(raw_expr.clone().separated_by(just(Token::Ctrl(','))))
            .then(expr.clone().or_not())
            .map_with_span(|(val, body), span: Span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span.clone()),
                };
                (Expr::Output(val, Box::new(body)), span)
            });

        let input = just(Token::Keyword("INPUT".to_string()))
            .ignore_then(ident)
            .then(
                just(Token::Ctrl('.'))
                    .ignore_then(
                        ident.map_with_span(|ident, span| (Expr::Value(Value::Str(ident)), span)),
                    )
                    .or(raw_expr
                        .clone()
                        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']'))))
                    .repeated(),
            )
            .then(expr.clone().or_not())
            .map_with_span(|((name, children), body), span: Span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span.clone()),
                };
                (
                    Expr::Input(
                        Box::new((Expr::Value(Value::Str(name)), span.clone())),
                        children,
                        Box::new(body),
                    ),
                    span,
                )
            });

        let declare_comp = just(Token::Keyword("TYPE".to_string()))
            .ignore_then(ident)
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
                    span,
                )
            });

        let return_ = just(Token::Keyword("RETURN".to_string()))
            .ignore_then(raw_expr.clone())
            .map_with_span(|a, span| (Expr::Return(Box::new(a)), span));

        let for_ = recursive(|for_| {
            just(Token::Keyword("FOR".to_string()))
                .ignore_then(assign.clone())
                .then_ignore(just(Token::Keyword("TO".to_string())))
                .then(raw_expr.clone())
                .then(
                    just(Token::Keyword("STEP".to_string()))
                        .ignore_then(raw_expr.clone())
                        .or_not(),
                )
                .then(block.clone().or(for_))
                .then_ignore(just(Token::Keyword("NEXT".to_string())))
                .then(raw_expr.clone())
                .then(expr.clone().or_not())
                .map_with_span(|(((((start, end), step), body), var), then), span: Span| {
                    (
                        Expr::For(
                            Box::new(start),
                            Box::new(end),
                            Box::new(match step {
                                Some(t) => t,
                                None => (Expr::Value(Value::Int(1)), span.clone()),
                            }),
                            Box::new(body),
                            Box::new(var),
                            Box::new(match then {
                                Some(t) => t,
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                        ),
                        span,
                    )
                })
        });

        let open_file = just(Token::Keyword("OPENFILE".to_string()))
            .ignore_then(raw_expr.clone())
            .then_ignore(just(Token::Keyword("FOR".to_string())))
            .then(one_of::<_, _, Simple<Token>>(vec![
                Token::Keyword("READ".to_string()),
                Token::Keyword("WRITE".to_string()),
                Token::Keyword("APPEND".to_string()),
            ]))
            .then(expr.clone().or_not())
            .map_with_span(|((file_name, file_mode), body), span| {
                (
                    Expr::OpenFile(
                        Box::new(file_name),
                        match file_mode {
                            Token::Keyword(type_) => match type_.as_str() {
                                "READ" => FileMode::Read,
                                "WRITE" => FileMode::Write,
                                "APPEND" => FileMode::Append,
                                _ => FileMode::Read,
                            },
                            _ => FileMode::Read,
                        },
                        Box::new(match body {
                            Some(t) => t,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            });

        let close_file = just(Token::Keyword("OPENFILE".to_string()))
            .ignore_then(raw_expr.clone())
            .then(expr.clone().or_not())
            .map_with_span(|(file_name, body), span| {
                (
                    Expr::CloseFile(
                        Box::new(file_name),
                        Box::new(match body {
                            Some(t) => t,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            });

        let write_file = just(Token::Keyword("WRITEFILE".to_string()))
            .ignore_then(raw_expr.clone())
            .then_ignore(just(Token::Ctrl(',')))
            .then(raw_expr.clone())
            .then(expr.clone().or_not())
            .map_with_span(|((file_name, data), body), span| {
                (
                    Expr::WriteFile(
                        Box::new(file_name),
                        Box::new(data),
                        Box::new(match body {
                            Some(t) => t,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            });

        let read_file = just(Token::Keyword("READFILE".to_string()))
            .ignore_then(raw_expr.clone())
            .then_ignore(just(Token::Ctrl(',')))
            .then(ident)
            .then(
                just(Token::Ctrl('.'))
                    .ignore_then(
                        ident.map_with_span(|ident, span| (Expr::Value(Value::Str(ident)), span)),
                    )
                    .or(raw_expr
                        .clone()
                        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']'))))
                    .repeated(),
            )
            .then(expr.clone().or_not())
            .map_with_span(|(((file_name, name), children), body), span: Span| {
                let body = match body {
                    Some(t) => t,
                    None => (Expr::Value(Value::Null), span.clone()),
                };
                (
                    Expr::ReadFile(
                        Box::new(file_name),
                        Box::new((Expr::Value(Value::Str(name)), span.clone())),
                        children,
                        Box::new(body),
                    ),
                    span,
                )
            });

        if_.or(function)
            .or(procedure)
            .or(call_procedure)
            .or(while_)
            .or(for_)
            .or(assign)
            .or(output)
            .or(input)
            .or(declare_comp)
            .or(declare_arr)
            .or(declare_prim)
            .or(return_)
            .or(open_file)
            .or(write_file)
            .or(read_file)
            .or(close_file)
            .labelled("block")
    })
    .then_ignore(end())
}
