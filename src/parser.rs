use std::collections::HashMap;

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
    Local(String),
    Array(Vec<Spanned<Self>>),
    DeclarePrim(String, String, Box<Option<Spanned<Self>>>),
    DeclareArr(
        String,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        String,
        Box<Option<Spanned<Self>>>,
    ),
    Assign(String, Box<Spanned<Self>>, Box<Option<Spanned<Self>>>),
    Index(
        String,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Option<Spanned<Self>>>,
    ),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    For(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    While(Box<Spanned<Self>>),
    Repeat(Box<Spanned<Self>>),
    Output(Box<Spanned<Self>>),
    Input(String),
}

pub fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
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
                .then(type_)
                .then(expr.clone().or_not())
                .map(|((name, t), body)| Expr::DeclarePrim(name, t, Box::new(body)))
                .labelled("Declare_prim");

            let declare_arr = just(Token::Keyword("DECLARE".to_string()))
                .ignore_then(ident)
                .then_ignore(just(Token::Ctrl(':')))
                .then_ignore(type_)
                .then_ignore(just(Token::Ctrl('[')))
                .then(raw_expr.clone())
                .then_ignore(just(Token::Ctrl(':')))
                .then(raw_expr.clone())
                .then_ignore(just(Token::Ctrl(']')))
                .then_ignore(just(Token::Keyword("OF".to_string())))
                .then(type_)
                .then(expr.clone().or_not())
                .map(|((((name, val1), val2), t), body)| {
                    Expr::DeclareArr(name, Box::new(val1), Box::new(val2), t, Box::new(body))
                })
                .labelled("Declare_arr");

            let assign = ident
                .clone()
                .then_ignore(just(Token::Op("<-".to_string())))
                .then(raw_expr.clone())
                .then(expr.clone().or_not())
                .map(|((name, v), body)| Expr::Assign(name, Box::new(v), Box::new(body)))
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
                .map(|(((name, v1), v2), body)| {
                    (Expr::Index(name, Box::new(v1), Box::new(v2), Box::new(body)))
                });

            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing();

            let list = items
                .clone()
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                .map(Expr::Array);

            let atom = val
                .or(declare_prim)
                .or(declare_arr)
                .or(assign)
                .or(index)
                .or(ident.map(Expr::Local))
                .or(list)
                .labelled("atom")
                .map_with_span(|expr, span| (expr, span))
                .or(expr.clone().delimited_by(
                    just(Token::Open(Delim::Paren)),
                    just(Token::Close(Delim::Paren)),
                ));

            let call = atom
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
                    just(Token::Keyword("Else".to_string()))
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
        //.then(expr.or_not().repeated())
        //.foldl(|a, b| {
        //    let a_start = a.1.start;
        //    let b_end = b.as_ref().map(|b| b.1.end).unwrap_or(a.1.end);
        //    (
        //        Expr::Then(
        //            Box::new(a),
        //            Box::new(match b {
        //                Some(b) => b,
        //                // Since there is no b expression then its span is empty.
        //                None => (Expr::Value(Value::Null), b_end..b_end),
        //            }),
        //        ),
        //        a_start..b_end,
        //    )
        //})
    })
    .then_ignore(end())
}
