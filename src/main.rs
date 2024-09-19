use std::env::args;
use std::fs;

#[macro_use]
mod macros;

pub mod file;
mod grammar;
mod lexer;
pub mod nodes;
mod parser;
mod peeking_chars;
mod syntax;

fn main() {
    let file = args().nth(1).unwrap();
    let input = fs::read_to_string(file).unwrap();

    parser::lex_and_parse(input.as_str());
}
