use std::{fmt::Display, iter::Peekable, str::Chars};

use crate::grammar::{FilePosition, FileSpan, Span, Token, TokenType};

/// A node of the syntax tree
#[derive(Debug)]
pub struct Node {
    node_type: NodeType,
    span: Option<Span>,
}

impl Node {
    /// Create a non-empty node.
    pub fn new(node_type: NodeType, span: Option<Span>) -> Self {
        let empty_span = span.is_none();
        let is_empty = matches!(node_type, NodeType::Empty(_));

        if is_empty && !empty_span {
            panic!("Non-empty nodes must have a span");
        } else if !is_empty && empty_span {
            panic!("Empty nodes must not have a span");
        }

        Self { node_type, span }
    }

    /// Create a non-empty node
    pub fn non_empty(node_type: NodeType, span: Span) -> Self {
        if matches!(node_type, NodeType::Empty(_)) {
            panic!("Cannot create a non-empty node that is an error");
        }

        Self {
            node_type,
            span: Some(span),
        }
    }

    /// Create an empty error node
    pub fn empty(node_type: EmptyNodeType) -> Self {
        Self {
            node_type: NodeType::Empty(node_type),
            span: None,
        }
    }

    /// Get the start of the span of the node, if it is not empty.
    pub fn start(&self) -> Option<usize> {
        self.span.as_ref().map(|x| x.start())
    }

    /// Get the end of the span of the node, if it is not empty.
    pub fn end(&self) -> Option<usize> {
        self.span.as_ref().map(|x| x.end())
    }

    pub fn node_type(&self) -> &NodeType {
        &self.node_type
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn children(&self) -> Vec<&Node> {
        self.node_type.children()
    }

    pub fn is_error(&self) -> bool {
        self.node_type.is_error()
    }

    pub fn is_empty(&self) -> bool {
        self.node_type.is_empty()
    }

    pub fn as_error(self) -> Self {
        if self.is_error() {
            return self;
        }

        let span = self.span.clone();
        Self {
            node_type: NodeType::ErrorBoundary {
                node: Box::new(self),
            },
            span,
        }
    }

    pub fn relevent_tokens<'a>(&self, tokens: &'a [Token]) -> Vec<&'a Token> {
        if self.is_empty() {
            panic!("Cannot call relevent_tokens on an empty node");
        }

        self.span
            .as_ref()
            .unwrap()
            .iter()
            .filter(|x| {
                self.children()
                    .iter()
                    .any(|y| y.span().is_some_and(|y| y.contains(*x)))
            })
            .map(|x| &tokens[x])
            .collect()
    }

    pub fn text_span(&self, tokens: &[Token]) -> Option<FileSpan> {
        if self.is_empty() {
            return None;
        }

        let tokens_slice = self.span.as_ref().unwrap().slice(tokens);

        match (tokens_slice.first(), tokens_slice.last()) {
            (Some(first), Some(last)) => Some(FileSpan::between_spans(first.span(), last.span())),
            _ => None,
        }
    }

    pub fn contains(&self, position: FilePosition, tokens: &[Token]) -> bool {
        self.text_span(tokens).is_some_and(|x| x.contains(position))
    }

    pub fn covers(&self, span: FileSpan, tokens: &[Token]) -> bool {
        self.text_span(tokens).is_some_and(|x| x.covers(span))
    }

    pub fn is_leaf(&self) -> bool {
        self.children().len() == 0
    }
}

#[derive(Debug)]
pub enum NodeType {
    Condition {
        expr: Box<Node>,
    },
    Symbol {
        value: String,
    },
    LiteralNumber,
    LiteralString {
        value: String,
    },
    LiteralBool {
        value: bool,
    },
    Null,
    Placeholder,
    PrefixCall {
        rhs: Box<Node>,
    },
    Parentheses {
        contents: Box<Node>,
    },
    Braces {
        exprs: Vec<Node>,
    },
    If {
        condition: Box<Node>,
        consequent_expr: Box<Node>,
        alternative_expr: Option<Box<Node>>,
    },
    For {
        condition: Box<Node>,
        expr: Box<Node>,
    },
    While {
        condition: Box<Node>,
        expr: Box<Node>,
    },
    Repeat {
        expr: Box<Node>,
    },
    Function {
        args: Box<Node>,
        body: Box<Node>,
    },
    Next,
    Break,
    Call {
        function: Box<Node>,
        args: Box<Node>,
    },
    Subset {
        lhs: Box<Node>,
        args: Box<Node>,
    },
    Index {
        lhs: Box<Node>,
        args: Box<Node>,
    },
    NameSpace {
        internal: bool,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Extract {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Binary {
        op: Token,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    ForCondition {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    FormList {
        items: Vec<Node>,
    },
    FormListItem {
        lhs: Box<Node>,
        rhs: Option<Box<Node>>,
    },
    SubList {
        items: Vec<Node>,
    },
    SubListItem {
        lhs: Box<Node>,
        rhs: Option<Box<Node>>,
    },
    WhiteSpace,
    File {
        exprs: Vec<Node>,
    },
    ErrorBoundary {
        node: Box<Node>,
    },
    Empty(EmptyNodeType),
}

impl NodeType {
    pub fn is_error(&self) -> bool {
        match self {
            NodeType::ErrorBoundary { .. } | NodeType::Empty(_) => true,
            _ => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            NodeType::Empty(_) => true,
            _ => false,
        }
    }

    pub fn children(&self) -> Vec<&Node> {
        match self {
            NodeType::Condition { expr } => vec![expr],
            NodeType::Symbol { value: _ } => vec![],
            NodeType::LiteralNumber => vec![],
            NodeType::LiteralString { value: _ } => vec![],
            NodeType::LiteralBool { value: _ } => vec![],
            NodeType::Null => vec![],
            NodeType::Placeholder => vec![],
            NodeType::PrefixCall { rhs } => vec![rhs],
            NodeType::Parentheses { contents } => vec![contents],
            NodeType::Braces { exprs } => exprs.iter().map(|item| item).collect(),
            NodeType::If {
                condition,
                consequent_expr,
                alternative_expr,
            } => {
                if let Some(alternative) = alternative_expr {
                    vec![condition, consequent_expr, alternative]
                } else {
                    vec![condition, consequent_expr]
                }
            }
            NodeType::For { condition, expr } => vec![condition, expr],
            NodeType::While { condition, expr } => vec![condition, expr],
            NodeType::Repeat { expr } => vec![expr],
            NodeType::Function { args, body: expr } => vec![args, expr],
            NodeType::Next => vec![],
            NodeType::Break => vec![],
            NodeType::Call { function, args } => vec![function, args],
            NodeType::Subset { lhs, args } => vec![lhs, args],
            NodeType::Index { lhs, args } => vec![lhs, args],
            NodeType::NameSpace {
                internal: _,
                lhs,
                rhs: args,
            } => vec![lhs, args],
            NodeType::Extract { lhs, rhs } => vec![lhs, rhs],
            NodeType::Binary { op: _, lhs, rhs } => vec![lhs, rhs],
            NodeType::ForCondition { lhs, rhs } => vec![lhs, rhs],
            NodeType::FormList { items } => items.iter().map(|item| item).collect(),
            NodeType::FormListItem { lhs, rhs } => {
                if let Some(rhs) = rhs {
                    vec![lhs, rhs]
                } else {
                    vec![lhs]
                }
            }
            NodeType::SubList { items } => items.iter().map(|item| item).collect(),
            NodeType::SubListItem { lhs, rhs } => {
                if let Some(rhs) = rhs {
                    vec![lhs, rhs]
                } else {
                    vec![lhs]
                }
            }
            NodeType::WhiteSpace => vec![],
            NodeType::File { exprs } => exprs.iter().map(|item| item).collect(),
            NodeType::ErrorBoundary { node } => vec![node],
            NodeType::Empty(_) => vec![],
        }
    }
}

#[derive(Debug)]
pub enum EmptyNodeType {
    Expr,
    File,
    FormList,
    SubList,
}

/// Parse a node that is made up of a single token (the leaves of the syntax tree).
pub fn atom(token: &Token) -> Option<NodeType> {
    match *token.token_type() {
        TokenType::Number | TokenType::NaN | TokenType::Inf | TokenType::NA(_) => {
            Some(NodeType::LiteralNumber)
        }
        TokenType::True | TokenType::False => Some(NodeType::LiteralBool {
            value: token.token_type() == &TokenType::True,
        }),
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

fn parse_string(token: &Token, symbol: bool) -> String {
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

fn parse_raw_string(token: &Token) -> String {
    let mut chars = token.content().chars();

    let mut count = 0;
    while chars.next().is_some_and(|x| "({[|".contains(x)) {
        count += 1;
    }

    let content = &token.content()[count..token.content().len() - count];

    content.to_string()
}

fn parse_symbol(token: &Token) -> String {
    if !token.content().starts_with('`') {
        return token.content().to_string();
    }

    parse_string(token, true)
}

fn parse_base(chars: &mut Peekable<Chars>, first: char, base: u32) -> u32 {
    let mut n = first.to_digit(base).unwrap();

    while let Some(x) = chars.peek().and_then(|x| x.to_digit(base)) {
        n = n * 8 + x;
        chars.next();
    }

    n
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            return Ok(());
        }

        let mut string = format!(
            "{:?} {}..{}\n",
            self.node_type,
            self.span.as_ref().unwrap().start(),
            self.span.as_ref().unwrap().end()
        );

        for child in self.children() {
            for line in child.to_string().lines() {
                string.push_str(format!("    {}\n", line).as_str())
            }
        }

        string.pop();

        write!(f, "{}", string)
    }
}
