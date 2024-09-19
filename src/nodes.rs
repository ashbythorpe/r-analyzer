use std::{fmt::Display, iter::Peekable, str::Chars};

use crate::grammar::{Span, Token, TokenType};

#[derive(Debug)]
pub struct Node {
    node_type: NodeType,
    span: Span,
    children: Vec<Node>,
    error: bool,
    empty: bool,
}

impl Node {
    pub fn new(node_type: NodeType, span: Span, children: Vec<Node>, error: bool) -> Self {
        Self {
            node_type,
            span,
            children,
            error,
            empty: false,
        }
    }

    pub fn ok(node_type: NodeType, span: Span, children: Vec<Node>) -> Self {
        Self {
            node_type,
            span,
            children,
            error: false,
            empty: false,
        }
    }

    pub fn error(node_type: NodeType, span: Span, children: Vec<Node>) -> Self {
        Self {
            node_type,
            span,
            children,
            error: true,
            empty: false,
        }
    }

    pub fn empty(node_type: NodeType) -> Self {
        Self {
            node_type,
            span: Span::single(0),
            children: vec![],
            error: true,
            empty: true,
        }
    }

    pub fn wraps(node_type: NodeType, child: Node) -> Self {
        Self {
            node_type,
            span: child.span().clone(),
            children: vec![child],
            error: false,
            empty: false,
        }
    }

    pub fn end(&self) -> Option<usize> {
        if self.children.is_empty() {
            Some(self.span.end())
        } else {
            None
        }
    }

    pub fn start(&self) -> Option<usize> {
        if self.children.is_empty() {
            Some(self.span.start())
        } else {
            None
        }
    }

    pub fn node_type(&self) -> &NodeType {
        &self.node_type
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn children(&self) -> &Vec<Node> {
        &self.children
    }

    pub fn is_error(&self) -> bool {
        self.error
    }

    pub fn is_empty(&self) -> bool {
        self.empty
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeType {
    WhiteSpace,
    Condition,
    Separator,
    ExprList,
    Expr,
    Symbol { value: String },
    LiteralNumber,
    LiteralString { value: String },
    Null,
    Placeholder,
    PrefixCall,
    InfixCall { value: Token },
    Parentheses,
    Braces,
    If,
    For,
    While,
    Repeat,
    Function,
    Next,
    Break,
    Call,
    Subset,
    Index,
    Assign,
    Help,
    NameSpace { internal: bool },
    Extract,
    Binary { op: Token },
    ForCondition,
    FormList,
    FormListItem,
    SubList,
    SubListItem,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeError {
    EmptyNode,
    UnclosedDelimiter,
}

pub fn atom(token: &Token) -> Option<NodeType> {
    match *token.token_type() {
        TokenType::Number
        | TokenType::NaN
        | TokenType::Inf
        | TokenType::NA(_)
        | TokenType::True
        | TokenType::False => Some(NodeType::LiteralNumber),
        TokenType::String => Some(NodeType::LiteralString {
            value: parse_string(token, false),
        }),
        TokenType::RawString => Some(NodeType::LiteralString {
            value: parse_raw_string(token),
        }),
        TokenType::Symbol => Some(NodeType::Symbol {
            value: parse_symbol(token),
        }),
        TokenType::Null => Some(NodeType::Null),
        TokenType::Placeholder => Some(NodeType::Placeholder),
        TokenType::Next => Some(NodeType::Next),
        TokenType::Break => Some(NodeType::Break),
        TokenType::Dots | TokenType::Dot => Some(NodeType::Symbol {
            value: token.content().to_string(),
        }),
        _ => None,
    }
}

pub fn parse_string(token: &Token, symbol: bool) -> String {
    let inner_content = &token.content()[1..token.content().len() - 1];

    let mut result = String::new();

    let mut chars = inner_content.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            let next = chars
                .next()
                .expect("Backslash must be followed by another character");

            if next.is_digit(8) {
                let n = parse_base(&mut chars, next, 8);
                result.push_str(n.to_string().as_str());
            } else if next == 'x' {
                let n = parse_base(&mut chars, next, 16);
                result.push_str(n.to_string().as_str());
            } else if next == 'u' || next == 'U' {
                if symbol {
                    panic!("Unicode sequences are not allowed inside backticks");
                }

                let n = parse_base(&mut chars, next, 16);
                let char = char::from_u32(n).unwrap();

                result.push(char);
            } else {
                let char = match next {
                    'a' => '\x07',
                    'b' => '\x08',
                    'f' => '\x0C',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'v' => '\x0B',
                    '\\' => '\\',
                    '\"' => '\"',
                    '\'' => '\'',
                    '`' => '`',
                    ' ' => ' ',
                    '\n' => '\n',
                    _ => panic!("Unrecognized escape sequence"),
                };

                result.push(char);
            }
        } else {
            result.push(c);
        }
    }

    result
}

pub fn parse_raw_string(token: &Token) -> String {
    let mut chars = token.content().chars();

    let mut count = 0;
    while chars.next().is_some_and(|x| "({[|".contains(x)) {
        count += 1;
    }

    let content = &token.content()[count..token.content().len() - count];

    content.to_string()
}

pub fn parse_symbol(token: &Token) -> String {
    if !token.content().starts_with('`') {
        return token.content().to_string();
    }

    parse_string(token, true)
}

pub fn parse_base(chars: &mut Peekable<Chars>, first: char, base: u32) -> u32 {
    let mut n = first.to_digit(base).unwrap();

    while let Some(x) = chars.peek().and_then(|x| x.to_digit(base)) {
        n = n * 8 + x;
        chars.next();
    }

    n
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut string = format!(
            "{:?} {}..{}\n",
            self.node_type,
            self.span.start(),
            self.span.end()
        );

        for child in &self.children {
            for line in child.to_string().lines() {
                string.push_str(format!("    {}\n", line).as_str())
            }
        }

        string.pop();

        write!(f, "{}", string)
    }
}
