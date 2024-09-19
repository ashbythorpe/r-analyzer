use std::iter::from_fn;

use crate::grammar::{Token, TokenType, NA};

use crate::peeking_chars::CharTraverser;

fn parse_number(input: &mut CharTraverser, starting_dot: bool) {
    let mut seen_dot = starting_dot;

    input.next_while(|x| {
        if x == '.' {
            if seen_dot {
                panic!("Must be a number");
            }
            seen_dot = true;
            return true;
        }

        x.is_ascii_digit()
    });

    if input.next_if(|x| x == 'x' || x == 'X').is_some() {
        let current = input.stored_string();
        if current.len() != 2 || !current.starts_with('0') {
            panic!("Must be a number");
        }
        parse_hex(input);
    } else if input.next_if(|x| x == 'e' || x == 'E').is_some() {
        parse_exp(input);
    }

    input.next_if(|x| x == 'i' || x == 'L');
}

fn parse_hex(input: &mut CharTraverser) {
    let mut seen_dot = false;

    input.next_while(|x| {
        if x == '.' {
            if seen_dot {
                panic!("Must be a number");
            }
            seen_dot = true;
            return true;
        }

        x.is_ascii_hexdigit()
    });

    if input.stored_string().len() == 2 {
        panic!("Must be a number");
    }

    if input.next_if(|x| x == 'p' || x == 'P').is_some() {
        parse_exp(input);
    }
}

fn parse_exp(input: &mut CharTraverser) {
    input.next_if(|x| x == '+' || x == '-');

    input.next_while(|x| x.is_ascii_digit());
}

fn parse_symbol_value(input: &mut CharTraverser) {
    input.next_while(|x| x.is_alphanumeric() || x == '.' || x == '_');
}

fn parse_raw_string(input: &mut CharTraverser, quote: char) {
    let dashes = input.count_next(|x| x == '-');

    let delim = match input.next() {
        Some(x) => match x {
            '(' => ')',
            '{' => '}',
            '[' => ']',
            '|' => '|',
            _ => panic!("Invalid delimiter"),
        },
        None => panic!("Unterminated raw string"),
    };

    loop {
        if input.next_if(|x| x == quote).is_some()
            && input.count_next(|x| x == '-') == dashes
            && input.next_if(|x| x == delim).is_some()
        {
            break;
        } else if input.next().is_none() {
            panic!("Unterminated raw string");
        }
    }
}

fn parse_string_value(input: &mut CharTraverser, quote: char) {
    // while let Some(&x) = input.peek() {
    //     if input.push_if(&mut text, |x| *x == quote) {
    //         break;
    //     } else if input.push_if(&mut text, |x| *x == '\n') {
    //         //
    //     } else if x == '\\' {
    //         let next = match input.peek() {
    //             Some(&x) => x,
    //             _ => panic!("Must be a char"),
    //         };
    //
    //         if next.is_digit(8) {
    //             // TODO: Handle octal
    //         } else if next == 'x' {
    //             // TODO: Handle hex
    //         } else if next == 'u' {
    //             // TODO: Handle unicode
    //         } else if next == 'U' {
    //             // TODO: Handle unicode again
    //         } else {
    //             let char = match next {
    //                 'a' => '\x07',
    //                 'b' => '\x08',
    //                 'f' => '\x0C',
    //                 'n' => '\n',
    //                 'r' => '\r',
    //                 't' => '\t',
    //                 'v' => '\x0B',
    //                 '\\' => '\\',
    //                 '\"' => '\"',
    //                 '\'' => '\'',
    //                 '`' => '`',
    //                 ' ' => ' ',
    //                 '\n' => '\n',
    //                 _ => panic!("Unrecognized escape sequence"),
    //             };
    //
    //             text.push(char);
    //             input.next();
    //         }
    //     } else {
    //         input.take_and_push(&mut text);
    //     }
    // }

    while let Some(x) = input.next() {
        if x == quote {
            break;
        } else if x == '\\' {
            input.next();
        }
    }
}

fn parse_special_value(input: &mut CharTraverser) {
    input.next_while(|x| x != '%' && x != '\n');

    if input.next_if(|x| x == '%').is_none() {
        panic!("Unterminated %");
    }
}

fn parse_simple_symbol(input: &mut CharTraverser, char: char) -> TokenType {
    match char {
        '<' => match input.next_if_matches("=-<") {
            Some('=') => TokenType::LessThanEquals,
            Some('-') => TokenType::LeftAssign,
            Some('<') => match input.next_if_matches("-") {
                Some('-') => TokenType::DoubleLeftAssign,
                _ => panic!("Must be a symbol"),
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
        _ => panic!("Unrecognized symbol"),
    }
}

fn get_token(input: &mut CharTraverser) -> Option<Token> {
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
                parse_number(input, true);
                TokenType::Number
            } else {
                parse_symbol_value(input);
                match_reserved(input.stored_string())
            }
        }
        x if x.is_ascii_digit() => {
            parse_number(input, false);
            TokenType::Number
        }
        '"' | '\'' => {
            parse_string_value(input, char);
            TokenType::String
        }
        '%' => {
            parse_special_value(input);
            TokenType::Infix
        }
        '`' => {
            parse_string_value(input, '`');
            TokenType::Symbol
        }
        '#' => {
            input.next_while(|x| x != '\n');
            TokenType::Comment
        }
        '_' | 'a'..='z' | 'A'..='Z' => {
            if char == 'r' || char == 'R' {
                if let Some(x) = input.next_if_matches("\\\"") {
                    parse_raw_string(input, x);
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
        x => parse_simple_symbol(input, x),
    };

    Some(Token::new(input.take_stored_string(), result_type))
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut traverser = CharTraverser::new(input);

    from_fn(|| get_token(&mut traverser)).collect::<Vec<Token>>()
}

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
