#[macro_export]
macro_rules! keywords {
    () => {
        TokenType::If
            | TokenType::Else
            | TokenType::Repeat
            | TokenType::While
            | TokenType::Function
            | TokenType::For
            | TokenType::In
            | TokenType::Next
            | TokenType::Break
            | TokenType::True
            | TokenType::False
            | TokenType::Null
            | TokenType::Inf
            | TokenType::NaN
            | TokenType::NA(_)
    };
}

#[macro_export]
macro_rules! infix_operators {
    () => {
        TokenType::LeftSquare
            | TokenType::LeftDoubleSquare
            | TokenType::Colon
            | TokenType::DoubleColon
            | TokenType::TripleColon
            | TokenType::Dollar
            | TokenType::At
            | TokenType::Caret
            | TokenType::DoubleAsterisk
            | TokenType::Minus
            | TokenType::Plus
            | TokenType::Pipe
            | TokenType::PipeBind
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Equals
            | TokenType::DoubleEquals
            | TokenType::NotEquals
            | TokenType::LessThan
            | TokenType::LessThanEquals
    };
}

macro_rules! prefix_operators {
    () => {
        TokenType::Plus | TokenType::Minus | TokenType::Tilde | TokenType::Not
    };
}

#[macro_export]
macro_rules! atoms {
    () => {
        TokenType::Number
            | TokenType::NaN
            | TokenType::Inf
            | TokenType::NA(_)
            | TokenType::True
            | TokenType::False
            | TokenType::String
            | TokenType::RawString
            | TokenType::Symbol
            | TokenType::Null
            | TokenType::Placeholder
            | TokenType::Next
            | TokenType::Break
            | TokenType::Dots
            | TokenType::Dot
    };
}
