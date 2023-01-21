mod lexer;
mod parser;
mod prelude;
use lexer::*;
use parser::*;
use prelude::*;

fn main() {
    let code = "DECLARE a : ARRAY[1:30] OF STRING\na[1] <- 0";

    let tts = lexer().parse(code).unwrap();

    println!("--- Token Trees ---\n{:#?}\n", tts);

    let eoi = 0..code.chars().count();
    let mut token_stream = tts_to_stream(eoi, tts);

    let flattened_trees = token_stream.fetch_tokens().collect::<Vec<_>>();

    println!("--- Flattened Token Trees ---\n{:?}\n", flattened_trees);

    let pst = expr_parser().parse(token_stream).unwrap();

    println!("--- Parse Tree ---\n{:?}", pst);
}
