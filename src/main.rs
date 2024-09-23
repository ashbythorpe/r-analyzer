use std::env::args;
use std::fs;
use std::io::{stdin, Read};

#[macro_use]
mod macros;

pub mod file;
mod format;
mod grammar;
mod lexer;
pub mod nodes;
mod parser;
mod peeking_chars;

fn main() {
    let arg = args().nth(1);

    let input = match arg {
        Some(file) => fs::read_to_string(file).unwrap(),
        None => {
            let mut input = String::new();
            stdin().read_to_string(&mut input).unwrap();
            input
        }
    };

    parser::lex_and_parse(input.as_str());
}
