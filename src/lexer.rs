use crate::prelude::*;
use std::fmt;

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
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Char(c) => write!(f, "{}", c),
            Token::Str(s) => write!(f, "{}", s),
            Token::Num(s) => write!(f, "{}", s),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Keyword(s) => write!(f, "{}", s),
            Token::Type(s) => write!(f, "{}", s),
            Token::Open(d) => match d {
                Delim::Paren => write!(f, "("),
                _ => write!(f, "{}", d),
            },
            Token::Close(d) => match d {
                Delim::Paren => write!(f, ")"),
                _ => write!(f, "{}", d),
            },
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delim {
    Paren,
    Block,
}

impl fmt::Display for Delim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Delim::Block => write!(f, "     "),
            _ => write!(f, ""),
        }
    }
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
            .then_ignore(just('\''))
            .map(Token::Char);
        let ctrl = one_of::<_, _, Simple<char>>(":,.[]").map(Token::Ctrl);

        let op = one_of::<_, _, Simple<char>>("+-*/=<>&")
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map(Token::Op);

        let ident = text::ident().map(|ident: String| match ident.as_str() {
            "FUNCTION" => Token::Keyword("FUNCTION".into()),
            "CALL" => Token::Keyword("CALL".into()),
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
            "STEP" => Token::Keyword("STEP".into()),
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
            "ENDTYPE" => Token::Keyword("ENDTYPE".into()),
            "OF" => Token::Keyword("OF".into()),
            "TYPE" => Token::Keyword("TYPE".into()),
            "ARRAY" => Token::Type("ARRAY".into()),
            "BOOLEAN" => Token::Type("BOOLEAN".into()),
            "INTEGER" => Token::Type("INTEGER".into()),
            "REAL" => Token::Type("REAL".into()),
            "DATE" => Token::Type("DATE".into()),
            "STRING" => Token::Type("STRING".into()),
            "OPENFILE" => Token::Keyword("OPENFILE".into()),
            "CLOSEFILE" => Token::Keyword("CLOSEFILE".into()),
            "WRITEFILE" => Token::Keyword("WRITEFILE".into()),
            "READFILE" => Token::Keyword("READFILE".into()),
            "READ" => Token::Keyword("READ".into()),
            "WRITE" => Token::Keyword("WRITE".into()),
            "APPEND" => Token::Keyword("APPEND".into()),
            _ => Token::Ident(ident),
        });

        let token = num
            .or(char_)
            .or(str_)
            .or(op.clone())
            .or(ident)
            .or(ctrl.clone())
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
