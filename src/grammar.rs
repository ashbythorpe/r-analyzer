/// Describes a token
#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Token {
    content: String,
    token_type: TokenType,
    span: FileSpan,
}

/// Describes a span of tokens
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    start: usize,
    end: usize,
}

/// Describes a span of text in a file
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FileSpan {
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
}

impl FileSpan {
    pub fn new(start_line: usize, start_column: usize, end_line: usize, end_column: usize) -> Self {
        Self {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }

    pub fn start(&self) -> (usize, usize) {
        (self.start_line, self.start_column)
    }

    pub fn end(&self) -> (usize, usize) {
        (self.end_line, self.end_column)
    }
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
    Error,       // Any other token
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
    pub fn new(content: String, token_type: TokenType, span: FileSpan) -> Self {
        Self {
            content,
            token_type,
            span,
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
