use std::{
    fmt::Display,
    iter::{once, FusedIterator, Once, Peekable},
    str::Chars,
};

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

    pub fn children(&self) -> NodesIter {
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
        expr: Vec<Node>,
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

    pub fn children(&self) -> NodesIter {
        match self {
            NodeType::Condition { expr } => NodesIter::one(expr),
            NodeType::Symbol { value: _ } => NodesIter::empty(),
            NodeType::LiteralNumber => NodesIter::empty(),
            NodeType::LiteralString { value: _ } => NodesIter::empty(),
            NodeType::LiteralBool { value: _ } => NodesIter::empty(),
            NodeType::Null => NodesIter::empty(),
            NodeType::Placeholder => NodesIter::empty(),
            NodeType::PrefixCall { rhs } => NodesIter::one(rhs),
            NodeType::Parentheses { contents } => NodesIter::one(contents),
            NodeType::Braces { exprs } => NodesIter::many(exprs),
            NodeType::If {
                condition,
                consequent_expr,
                alternative_expr,
            } => {
                if let Some(alternative) = alternative_expr {
                    NodesIter::three(condition, consequent_expr, alternative)
                } else {
                    NodesIter::two(condition, consequent_expr)
                }
            }
            NodeType::For { condition, expr } => NodesIter::two(condition, expr),
            NodeType::While { condition, expr } => NodesIter::two(condition, expr),
            NodeType::Repeat { expr } => NodesIter::one(expr),
            NodeType::Function { args, body: expr } => NodesIter::two(args, expr),
            NodeType::Next => NodesIter::empty(),
            NodeType::Break => NodesIter::empty(),
            NodeType::Call { function, args } => NodesIter::two(function, args),
            NodeType::Subset { lhs, args } => NodesIter::two(lhs, args),
            NodeType::Index { lhs, args } => NodesIter::two(lhs, args),
            NodeType::NameSpace {
                internal: _,
                lhs,
                rhs: args,
            } => NodesIter::two(lhs, args),
            NodeType::Extract { lhs, rhs } => NodesIter::two(lhs, rhs),
            NodeType::Binary { op: _, lhs, rhs } => NodesIter::two(lhs, rhs),
            NodeType::ForCondition { lhs, rhs } => NodesIter::two(lhs, rhs),
            NodeType::FormList { items } => NodesIter::many(items),
            NodeType::FormListItem { lhs, rhs } => {
                if let Some(rhs) = rhs {
                    NodesIter::two(lhs, rhs)
                } else {
                    NodesIter::one(lhs)
                }
            }
            NodeType::SubList { items } => NodesIter::many(items),
            NodeType::SubListItem { lhs, rhs } => {
                if let Some(rhs) = rhs {
                    NodesIter::two(lhs, rhs)
                } else {
                    NodesIter::one(lhs)
                }
            }
            NodeType::WhiteSpace => NodesIter::empty(),
            NodeType::File { expr } => NodesIter::many(expr),
            NodeType::ErrorBoundary { node } => NodesIter::one(node),
            NodeType::Empty(_) => NodesIter::empty(),
        }
    }
}

pub struct NodesIter<'a>(NodesIterInner<'a>);

enum NodesIterInner<'a> {
    Empty(),
    One(Once<&'a Node>),
    Two {
        a: &'a Node,
        b: &'a Node,
        index: usize,
    },
    Three {
        a: &'a Node,
        b: &'a Node,
        c: &'a Node,
        index: usize,
    },
    Many {
        nodes: &'a Vec<Node>,
        index: usize,
    },
}

impl<'a> NodesIter<'a> {
    fn empty() -> Self {
        Self(NodesIterInner::Empty())
    }

    fn one(node: &'a Node) -> Self {
        Self(NodesIterInner::One(once(node)))
    }

    fn two(node1: &'a Node, node2: &'a Node) -> Self {
        Self(NodesIterInner::Two {
            a: node1,
            b: node2,
            index: 0,
        })
    }

    fn three(node1: &'a Node, node2: &'a Node, node3: &'a Node) -> Self {
        Self(NodesIterInner::Three {
            a: node1,
            b: node2,
            c: node3,
            index: 0,
        })
    }

    fn many(nodes: &'a Vec<Node>) -> Self {
        Self(NodesIterInner::Many { nodes, index: 0 })
    }
}

impl<'a> Iterator for NodesIter<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Iterator for NodesIterInner<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            NodesIterInner::Empty() => None,
            NodesIterInner::One(iter) => iter.next(),
            NodesIterInner::Two { a, b, index } => {
                if *index < 2 {
                    *index += 1;
                    Some(if *index == 1 { b } else { a })
                } else {
                    None
                }
            }
            NodesIterInner::Three { a, b, c, index } => {
                if *index < 3 {
                    *index += 1;
                    Some(if *index == 1 {
                        b
                    } else if *index == 2 {
                        c
                    } else {
                        a
                    })
                } else {
                    None
                }
            }
            NodesIterInner::Many { nodes, index } => {
                if *index < nodes.len() {
                    let node = &nodes[*index];
                    *index += 1;
                    Some(node)
                } else {
                    None
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            NodesIterInner::Empty() => (0, Some(0)),
            NodesIterInner::One(iter) => iter.size_hint(),
            NodesIterInner::Two { a: _, b: _, index } => (2 - *index, Some(2 - *index)),
            NodesIterInner::Three {
                a: _,
                b: _,
                c: _,
                index,
            } => (3 - *index, Some(3 - *index)),
            NodesIterInner::Many { nodes, index } => {
                let len = nodes.len() - *index;
                (len, Some(len))
            }
        }
    }
}

impl ExactSizeIterator for NodesIter<'_> {}

impl ExactSizeIterator for NodesIterInner<'_> {}

impl FusedIterator for NodesIter<'_> {}

impl FusedIterator for NodesIterInner<'_> {}

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
