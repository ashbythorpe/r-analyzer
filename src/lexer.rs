use std::iter::from_fn;

use ropey::Rope;

use crate::grammar::{Token, TokenType, NA};

use crate::char_traverser::CharTraverser;

pub struct LexerError {
    pub message: &'static str,
    pub index: usize,
}

/// Parse a number
fn parse_number(input: &mut CharTraverser) {
    input.next_while(|x| x.is_ascii_digit() || x == '.');

    if input.next_if(|x| x == 'x' || x == 'X').is_some() {
        parse_hex(input);
    } else if input.next_if(|x| x == 'e' || x == 'E').is_some() {
        parse_exp(input);
    }

    input.next_if(|x| x == 'i' || x == 'L');
}

/// Parse a hexadecimal number
fn parse_hex(input: &mut CharTraverser) {
    input.next_while(|x| x.is_ascii_hexdigit() || x == '.');

    if input.next_if(|x| x == 'p' || x == 'P').is_some() {
        parse_exp(input);
    }
}

/// Parse a number in E notation.
fn parse_exp(input: &mut CharTraverser) {
    input.next_if(|x| x == '+' || x == '-');

    input.next_while(|x| x.is_ascii_digit());
}

/// Parse a symbol
fn parse_symbol_value(input: &mut CharTraverser) {
    input.next_while(|x| x.is_alphanumeric() || x == '.' || x == '_');
}

/// Parse a raw string (e.g. r"(hello)")
fn parse_raw_string(
    input: &mut CharTraverser,
    errors: &mut Vec<LexerError>,
    i: usize,
    quote: char,
) {
    let dashes = input.count_next(|x| x == '-');

    let delim = match input.next() {
        Some(x) => match x {
            '(' => ')',
            '{' => '}',
            '[' => ']',
            '|' => '|',
            _ => {
                errors.push(LexerError {
                    message: "Invalid raw string delimiter",
                    index: i,
                });

                parse_string_value(input, errors, i, quote);
                return;
            }
        },
        None => {
            errors.push(LexerError {
                message: "Unterminated raw string",
                index: i,
            });

            return;
        }
    };

    loop {
        if input.next_if(|x| x == quote).is_some()
            && input.count_next(|x| x == '-') == dashes
            && input.next_if(|x| x == delim).is_some()
        {
            break;
        } else if input.next().is_none() {
            errors.push(LexerError {
                message: "Unterminated raw string",
                index: i,
            });

            return;
        }
    }
}

/// Parse a string. This is also used to parse a symbol wrapped in backticks.
fn parse_string_value(
    input: &mut CharTraverser,
    errors: &mut Vec<LexerError>,
    i: usize,
    quote: char,
) {
    loop {
        if let Some(x) = input.next() {
            if x == quote {
                break;
            } else if x == '\\' {
                input.next();
            }
        } else {
            errors.push(LexerError {
                message: "Unterminated string",
                index: i,
            });

            return;
        }
    }
}

/// Parse a special symbol (`%{x}%`)
fn parse_special_value(input: &mut CharTraverser, errors: &mut Vec<LexerError>, i: usize) {
    input.next_while(|x| x != '%' && x != '\n');

    if input.next_if(|x| x == '%').is_none() {
        errors.push(LexerError {
            message: "Unterminated %",
            index: i,
        });
    }
}

/// Parse a simple symbol (`+`, `<-`, etc.)
fn parse_simple_symbol(
    input: &mut CharTraverser,
    errors: &mut Vec<LexerError>,
    i: usize,
    char: char,
) -> TokenType {
    match char {
        '<' => match input.next_if_matches("=-<") {
            Some('=') => TokenType::LessThanEquals,
            Some('-') => TokenType::LeftAssign,
            Some('<') => match input.next_if_matches("-") {
                Some('-') => TokenType::DoubleLeftAssign,
                _ => {
                    errors.push(LexerError {
                        message: "Invalid symbol",
                        index: i,
                    });
                    TokenType::Error
                }
            },
            _ => TokenType::LessThan,
        },
        '>' => match input.next_if_matches("=") {
            Some('=') => TokenType::GreaterThanEquals,
            _ => TokenType::GreaterThan,
        },
        '!' => match input.next_if_matches("=") {
            Some('=') => TokenType::NotEquals,
            _ => TokenType::Not,
        },
        '=' => match input.next_if_matches("=>") {
            Some('=') => TokenType::DoubleEquals,
            Some('>') => TokenType::PipeBind,
            _ => TokenType::Equals,
        },
        ':' => match input.next_if_matches("=:") {
            Some('=') => TokenType::ColonEquals,
            Some(':') => match input.next_if_matches(":") {
                Some(':') => TokenType::TripleColon,
                _ => TokenType::DoubleColon,
            },
            _ => TokenType::Colon,
        },
        '&' => match input.next_if_matches("&") {
            Some('&') => TokenType::DoubleAnd,
            _ => TokenType::And,
        },
        '|' => match input.next_if_matches("|>") {
            Some('|') => TokenType::DoubleOr,
            Some('>') => TokenType::Pipe,
            _ => TokenType::Or,
        },
        '(' => TokenType::LeftParen,
        ')' => TokenType::RightParen,
        '{' => TokenType::LeftCurly,
        '}' => TokenType::RightCurly,
        '[' => match input.next_if_matches("[]") {
            Some('[') => TokenType::LeftDoubleSquare,
            _ => TokenType::LeftSquare,
        },
        ']' => TokenType::RightSquare,
        '?' => TokenType::Help,
        '*' => match input.next_if_matches("*") {
            Some('*') => TokenType::DoubleAsterisk,
            _ => TokenType::Asterisk,
        },
        '-' => match input.next_if_matches(">") {
            Some('>') => match input.next_if_matches(">") {
                Some('>') => TokenType::DoubleRightAssign,
                _ => TokenType::RightAssign,
            },
            _ => TokenType::Minus,
        },
        '+' => TokenType::Plus,
        '/' => TokenType::Slash,
        '^' => TokenType::Caret,
        '~' => TokenType::Tilde,
        '$' => TokenType::Dollar,
        '@' => TokenType::At,
        '\\' => TokenType::BackSlash,
        ',' => TokenType::Comma,
        ';' => TokenType::SemiColon,
        _ => {
            errors.push(LexerError {
                message: "Invalid symbol",
                index: i,
            });
            TokenType::Error
        }
    }
}

/// Get the next token
fn get_token(input: &mut CharTraverser, errors: &mut Vec<LexerError>, i: usize) -> Option<Token> {
    let char = input.next()?;

    let result_type = match char {
        '\n' => TokenType::NewLine,
        '\r' => {
            input.next_if(|x| x == '\n');
            TokenType::NewLine
        }
        ' ' => {
            input.next_while(|x| x.is_whitespace());
            TokenType::WhiteSpace
        }
        '.' => {
            if input.next_if(|x| x.is_ascii_digit()).is_some() {
                parse_number(input);
                TokenType::Number
            } else {
                parse_symbol_value(input);
                match_reserved(input.stored_string())
            }
        }
        x if x.is_ascii_digit() => {
            parse_number(input);
            TokenType::Number
        }
        '"' | '\'' => {
            parse_string_value(input, errors, i, char);
            TokenType::String
        }
        '%' => {
            parse_special_value(input, errors, i);
            TokenType::Infix
        }
        '`' => {
            parse_string_value(input, errors, i, '`');
            TokenType::Symbol
        }
        '#' => {
            input.next_while(|x| x != '\n');
            TokenType::Comment
        }
        '_' | 'a'..='z' | 'A'..='Z' => {
            if char == 'r' || char == 'R' {
                if let Some(x) = input.next_if_matches("\\\"") {
                    parse_raw_string(input, errors, i, x);
                    TokenType::RawString
                } else {
                    parse_symbol_value(input);
                    match_reserved(input.stored_string())
                }
            } else {
                parse_symbol_value(input);
                match_reserved(input.stored_string())
            }
        }
        x => parse_simple_symbol(input, errors, i, x),
    };

    let (string, span) = input.take_stored_string();
    Some(Token::new(string, result_type, span))
}

/// Tokenizes the input string, including whitespace tokens
pub fn lex(input: &Rope) -> (Vec<Token>, Vec<LexerError>) {
    let mut traverser = CharTraverser::new(input);
    let mut errors = Vec::new();
    let mut i = 0;

    (
        from_fn(|| {
            let result = get_token(&mut traverser, &mut errors, i);
            i += 1;
            result
        })
        .collect(),
        errors,
    )
}

/// Check if a symbol is a reserved word
fn match_reserved(text: &str) -> TokenType {
    match text {
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "repeat" => TokenType::Repeat,
        "while" => TokenType::While,
        "function" => TokenType::Function,
        "for" => TokenType::For,
        "in" => TokenType::In,
        "next" => TokenType::Next,
        "break" => TokenType::Break,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "NULL" => TokenType::Null,
        "Inf" => TokenType::Inf,
        "NaN" => TokenType::NaN,
        "NA" => TokenType::NA(NA::Default),
        "NA_integer_" => TokenType::NA(NA::NaInteger),
        "NA_real_" => TokenType::NA(NA::NaReal),
        "NA_complex_" => TokenType::NA(NA::NaComplex),
        "NA_character_" => TokenType::NA(NA::NaCharacter),
        "..." => TokenType::Dots,
        "_" => TokenType::Placeholder,
        _ => {
            if text.starts_with("..")
                && text[2..].chars().all(|x| x.is_ascii_digit())
                && text[2..].parse::<usize>().is_ok()
            {
                return TokenType::Dot;
            }

            TokenType::Symbol
        }
    }
}
