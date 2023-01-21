use chumsky::text::{newline, Character};

use crate::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Char(char),
    Str(String),
    Num(String),
    Bool(bool),
    Op(String),
    Ctrl(char),
    Ident(String),
    Keyword(String),
    Type(String),
    Open(Delim),
    Close(Delim),
    End,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delim {
    Paren,
    Block,
}

#[derive(Debug, PartialEq)]
pub enum TokenTree {
    Token(Token),
    Tree(Delim, Vec<Spanned<TokenTree>>),
}

pub fn lexer() -> impl Parser<char, Vec<Spanned<TokenTree>>, Error = Simple<char>> {
    let tt = recursive(|tt| {
        let num = text::int(10)
            .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
            .collect::<String>()
            .map(Token::Num);

        let str_ = just('"')
            .ignore_then(filter(|c| *c != '"').repeated())
            .then_ignore(just('"'))
            .collect::<String>()
            .map(Token::Str);

        let char_ = just::<_, _, Simple<char>>('\'')
            .ignore_then(filter(|c| *c != '\''))
            .then_ignore(just('"'))
            .map(Token::Char);
        let ctrl = one_of::<_, _, Simple<char>>(":,.[]").map(|c| Token::Ctrl(c));

        let op = one_of::<_, _, Simple<char>>("+-*/=<>&")
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map(Token::Op);

        let ident = text::ident().map(|ident: String| match ident.as_str() {
            "FUNCTION" => Token::Keyword("FUNCTION".into()),
            "RETURNS" => Token::Keyword("RETURNS".into()),
            "ENDFUNCTION" => Token::Keyword("ENDFUNCTION".into()),
            "DECLARE" => Token::Keyword("DECLARE".into()),
            "OUTPUT" => Token::Keyword("OUTPUT".into()),
            "INPUT" => Token::Keyword("INPUT".into()),
            "IF" => Token::Keyword("IF".into()),
            "ELSE" => Token::Keyword("ELSE".into()),
            "THEN" => Token::Keyword("THEN".into()),
            "ENDIF" => Token::Keyword("ENDIF".into()),
            "TRUE" => Token::Bool(true),
            "FALSE" => Token::Bool(false),
            "FOR" => Token::Keyword("FOR".into()),
            "TO" => Token::Keyword("TO".into()),
            "NEXT" => Token::Keyword("NEXT".into()),
            "WHILE" => Token::Keyword("WHILE".into()),
            "ENDWHILE" => Token::Keyword("ENDWHILE".into()),
            "REPEAT" => Token::Keyword("REPEAT".into()),
            "UNTIL" => Token::Keyword("UNTIL".into()),
            "PROCEDURE" => Token::Keyword("PROCEDURE".into()),
            "ENDPROCEDURE" => Token::Keyword("ENDPROCEDURE".into()),
            "RETURN" => Token::Keyword("RETURN".into()),
            "BYREF" => Token::Keyword("BYREF".into()),
            "BYVAL" => Token::Keyword("BYVAL".into()),
            "CASE OF" => Token::Keyword("CASE OF".into()),
            "OTHERWISE" => Token::Keyword("OTHERWISE".into()),
            "TYPE" => Token::Keyword("TYPE".into()),
            "ENDTYPE" => Token::Keyword("ENDTYPE".into()),
            "OF" => Token::Keyword("OF".into()),
            "ARRAY" => Token::Type("ARRAY".into()),
            "BOOLEAN" => Token::Type("BOOLEAN".into()),
            "INTEGER" => Token::Type("INTEGER".into()),
            "REAL" => Token::Type("REAL".into()),
            "DATE" => Token::Type("DATE".into()),
            "STRING" => Token::Type("STRING".into()),
            _ => Token::Ident(ident),
        });

        let end = end::<Simple<char>>().to(Token::End);

        let token = num
            .clone()
            .or(char_.clone())
            .or(str_.clone())
            .or(op.clone())
            .or(ident.clone())
            .or(ctrl.clone())
            //.or(end)
            .map(TokenTree::Token);

        let token_tree = tt
            .padded()
            .repeated()
            .delimited_by(just('('), just(')'))
            .map(|tts| TokenTree::Tree(Delim::Paren, tts));

        token.or(token_tree).map_with_span(|tt, span| (tt, span))
    });

    text::semantic_indentation(tt, |tts, span| (TokenTree::Tree(Delim::Block, tts), span))
        .then_ignore(end())
}

pub fn tts_to_stream(
    eoi: Span,
    token_trees: Vec<Spanned<TokenTree>>,
) -> BoxStream<'static, Token, Span> {
    use std::iter::once;

    BoxStream::from_nested(eoi, token_trees.into_iter(), |(tt, span)| match tt {
        // Single tokens remain unchanged
        TokenTree::Token(token) => Flat::Single((token, span)),
        // Nested token trees get flattened into their inner contents, surrounded by `Open` and `Close` tokens
        TokenTree::Tree(delim, tree) => Flat::Many(
            once((TokenTree::Token(Token::Open(delim)), span.clone()))
                .chain(tree.into_iter())
                .chain(once((TokenTree::Token(Token::Close(delim)), span))),
        ),
    })
}

#[test]
fn test_lexer() {
    let code = "DECLARE a INTEGER\na <- (10+5)*(10+1)\nIF a < 1\n  THEN\n      OUTPUT a\nENDIF";
    let tts = lexer().parse(code).unwrap();
    assert!(
        tts == vec![
            (
                TokenTree::Token(Token::Keyword("DECLARE".to_string())),
                0 as usize..7
            ),
            (TokenTree::Token(Token::Ident("a".to_string())), 8..9),
            (TokenTree::Token(Token::Type("INTEGER".to_string())), 10..17),
            (TokenTree::Token(Token::Ident("a".to_string())), 18..19),
            (TokenTree::Token(Token::Op("<-".to_string())), 20..22),
            (
                TokenTree::Tree(
                    Delim::Paren,
                    vec![
                        (TokenTree::Token(Token::Num("10".to_string())), 24..26),
                        (TokenTree::Token(Token::Op("+".to_string())), 26..27),
                        (TokenTree::Token(Token::Num("5".to_string())), 27..28)
                    ]
                ),
                23..29
            ),
            (TokenTree::Token(Token::Op("*".to_string())), 29..30),
            (
                TokenTree::Tree(
                    Delim::Paren,
                    vec![
                        (TokenTree::Token(Token::Num("10".to_string())), 31..33),
                        (TokenTree::Token(Token::Op("+".to_string())), 33..34),
                        (TokenTree::Token(Token::Num("1".to_string())), 34..35)
                    ]
                ),
                30..36
            ),
            (TokenTree::Token(Token::Keyword("IF".to_string())), 37..39),
            (TokenTree::Token(Token::Ident("a".to_string())), 40..41),
            (TokenTree::Token(Token::Op("<".to_string())), 42..43),
            (TokenTree::Token(Token::Num("1".to_string())), 44..45),
            (
                TokenTree::Tree(
                    Delim::Block,
                    vec![
                        (TokenTree::Token(Token::Keyword("THEN".to_string())), 48..52),
                        (
                            TokenTree::Tree(
                                Delim::Block,
                                vec![
                                    (
                                        TokenTree::Token(Token::Keyword("OUTPUT".to_string())),
                                        59..65
                                    ),
                                    (TokenTree::Token(Token::Ident("a".to_string())), 66..67)
                                ]
                            ),
                            59..67
                        )
                    ]
                ),
                48..52
            ),
            (
                TokenTree::Token(Token::Keyword("ENDIF".to_string())),
                68..73
            )
        ]
    );
}
