use std::env::args;
use std::fs::File;
use std::io::stdin;

use ropey::Rope;

#[macro_use]
mod macros;

mod char_traverser;
pub mod file;
mod format;
mod grammar;
mod lexer;
pub mod nodes;
mod parser;

fn main() -> std::io::Result<()> {
    let arg = args().nth(1);

    let input = match arg {
        Some(file) => Rope::from_reader(File::open(file)?)?,
        None => Rope::from_reader(stdin())?,
    };

    parser::lex_and_parse(&input);

    Ok(())
}
