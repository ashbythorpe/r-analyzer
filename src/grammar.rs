#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Token {
    content: String,
    token_type: TokenType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    WhiteSpace, // Any non-newline whitespace
    NewLine,
    Number,            // Literal numbers
    Symbol,            // A symbol, which can be wrapped in ``
    RawString,         // Literal raw strings
    String,            // Literal strings
    LeftParen,         // (
    RightParen,        // (
    LeftCurly,         // {
    RightCurly,        // }
    LeftSquare,        // [
    LeftDoubleSquare,  // [[
    RightSquare,       // ]
    Comma,             // ,
    If,                // if
    Else,              // else
    Repeat,            // repeat
    While,             // while
    Function,          // function
    For,               // for
    In,                // in
    Next,              // next
    Break,             // break
    True,              // TRUE
    False,             // FALSE
    Null,              // NULL
    Inf,               // Inf
    NaN,               // NaN
    NA(NA),            // NA or NA_integer_, NA_real_, NA_complex_, NA_character_, NA_logical
    Dots,              // ...
    Dot,               // ..1, ..2, etc
    Colon,             // :
    DoubleColon,       // ::
    TripleColon,       // :::
    Dollar,            // $
    At,                // @
    Caret,             // ^
    DoubleAsterisk,    // **, used as an alternative to ^
    Minus,             // -
    Plus,              // +
    Pipe,              // |>
    Placeholder,       // _
    Asterisk,          // *
    Slash,             // /
    Equals,            // =
    DoubleEquals,      // ==
    NotEquals,         // !=
    LessThan,          // <
    LessThanEquals,    // <=
    GreaterThan,       // >
    GreaterThanEquals, // >=
    Not,               // !
    And,               // &
    DoubleAnd,         // &&
    Or,                // |
    DoubleOr,          // ||
    Tilde,             // ~
    RightAssign,       // <-
    DoubleRightAssign, // <<-
    LeftAssign,        // <-
    DoubleLeftAssign,  // <<-
    Help,              // ?
    BackSlash,         // \ , for lambda functions \(args) body
    SemiColon,         // ;
    Infix,             // %any%
    PipeBind,          // =>, used with _R_USE_PIPEBIND_ to do e.g. thing |> x => x + 1
    ColonEquals, // :=, a deprecated alternative to <-, kept as a symbol for backward compatibility
    Comment,     // #
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NA {
    Default,
    NaInteger,
    NaReal,
    NaComplex,
    NaCharacter,
}

impl Token {
    pub fn new(content: String, token_type: TokenType) -> Self {
        Self {
            content,
            token_type,
        }
    }

    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn is_keyword(&self) -> bool {
        matches!(self.token_type, keywords!())
    }

    pub fn is_prefix_operator(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Plus | TokenType::Minus | TokenType::Tilde | TokenType::Not
        )
    }

    pub fn is_infix_operator(&self) -> bool {
        matches!(
            self.token_type,
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
                | TokenType::GreaterThan
                | TokenType::GreaterThanEquals
                | TokenType::Not
                | TokenType::And
                | TokenType::DoubleAnd
                | TokenType::Or
                | TokenType::DoubleOr
                | TokenType::Tilde
                | TokenType::RightAssign
                | TokenType::DoubleRightAssign
                | TokenType::LeftAssign
                | TokenType::DoubleLeftAssign
                | TokenType::ColonEquals
                | TokenType::Help
        )
    }

    // pub fn length(&self) -> usize {
    //     match self {
    //         Token::WhiteSpace(x) => x.len(),
    //         Token::NewLine => 1,
    //         Token::Number(x) => x.len(),
    //         Token::Symbol(x) => x.len(),
    //         Token::RawString(x) => x.len(),
    //         Token::String(x) => x.len(),
    //         Token::LeftBrace => 1,
    //         Token::RightBrace => 1,
    //         Token::LeftCurly => 1,
    //         Token::RightCurly => 1,
    //         Token::LeftSquare => 1,
    //         Token::LeftDoubleSquare => 2,
    //         Token::RightSquare => 1,
    //         Token::Comma => 1,
    //         Token::If => 2,
    //         Token::Else => 4,
    //         Token::Repeat => 6,
    //         Token::While => 5,
    //         Token::Function => 8,
    //         Token::For => 3,
    //         Token::In => 2,
    //         Token::Next => 4,
    //         Token::Break => 5,
    //         Token::True => 4,
    //         Token::False => 5,
    //         Token::Null => 4,
    //         Token::Inf => 3,
    //         Token::NaN => 3,
    //         Token::NA(NA::Default) => 2,
    //         Token::NA(NA::NaInteger) => 11,
    //         Token::NA(NA::NaReal) => 8,
    //         Token::NA(NA::NaComplex) => 11,
    //         Token::NA(NA::NaCharacter) => 13,
    //         Token::Dots => 3,
    //         Token::Dot(x) => 2 + x.to_string().len(),
    //         Token::Colon => 1,
    //         Token::DoubleColon => 2,
    //         Token::TripleColon => 3,
    //         Token::Dollar => 1,
    //         Token::At => 1,
    //         Token::Caret => 1,
    //         Token::DoubleAsterisk => 2,
    //         Token::Minus => 1,
    //         Token::Plus => 1,
    //         Token::Pipe => 2,
    //         Token::Asterisk => 1,
    //         Token::Slash => 1,
    //         Token::Equals => 1,
    //         Token::DoubleEquals => 2,
    //         Token::NotEquals => 2,
    //         Token::LessThan => 1,
    //         Token::LessThanEquals => 2,
    //         Token::GreaterThan => 1,
    //         Token::GreaterThanEquals => 2,
    //         Token::Not => 1,
    //         Token::And => 1,
    //         Token::DoubleAnd => 2,
    //         Token::Or => 1,
    //         Token::DoubleOr => 2,
    //         Token::Tilde => 1,
    //         Token::RightAssign => 2,
    //         Token::DoubleRightAssign => 3,
    //         Token::LeftAssign => 2,
    //         Token::DoubleLeftAssign => 3,
    //         Token::Help => 1,
    //         Token::BackSlash => 1,
    //         Token::SemiColon => 1,
    //         Token::Infix(x) => 2 + x.len(),
    //         Token::PipeBind => 2,
    //         Token::ColonEquals => 2,
    //     }
    // }

    pub fn is_literal(&self) -> bool {
        matches!(
            self.token_type(),
            TokenType::Number
                | TokenType::Symbol
                | TokenType::RawString
                | TokenType::String
                | TokenType::True
                | TokenType::False
                | TokenType::Null
                | TokenType::Inf
                | TokenType::NaN
                | TokenType::NA(_)
        )
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn single(x: usize) -> Self {
        Self { start: x, end: x }
    }

    pub fn between(x: &Span, y: &Span) -> Self {
        Self {
            start: x.start,
            end: y.end,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn slice<'a, T>(&self, x: &'a [T]) -> &'a [T] {
        &x[self.start..self.end]
    }
}
