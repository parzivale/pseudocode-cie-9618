mod lexer;
mod parser;
mod prelude;
use lexer::*;
use parser::*;
use prelude::*;

fn main() {
    let code = "DECLARE a:ARRAY[1:30] OF STRING\nDECLARE n:INTEGER\nTYPE m\n    DECLARE subm : INTEGER\nENDTYPE\na[1] <- 0\ni <- ((1+2+1-2)*4)\nIF i < 1\n     THEN\n      i <- i + 1\n     ELSE\n      i <- 1\nENDIF";

    let tts = lexer().parse(code).unwrap();

    println!("--- Token Trees ---\n{:#?}\n", tts);

    let eoi = 0..code.chars().count();
    let mut token_stream = tts_to_stream(eoi, tts);

    let flattened_trees = token_stream.fetch_tokens().collect::<Vec<_>>();

    println!("--- Flattened Token Trees ---\n{:?}\n", flattened_trees);

    let pst = expr_parser().parse(token_stream).unwrap();

    println!("--- Parse Tree ---\n{:#?}", pst);
}
