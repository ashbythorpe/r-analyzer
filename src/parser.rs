use core::panic;
use std::iter::{Enumerate, Peekable};
use std::slice::Iter;

use crate::grammar::{Span, Token, TokenType};

use crate::lexer::lex;
use crate::nodes::{self, Node, NodeType};

type Tokens<'a> = Peekable<Enumerate<Iter<'a, Token>>>;

#[derive(PartialEq, Eq, Clone)]
enum ClosingDelim {
    Paren,
    Square,
    Curly,
    Else,
    Comma,
    In,
}

impl ClosingDelim {
    fn closes(token_type: &TokenType) -> Option<Self> {
        match *token_type {
            TokenType::RightParen => Some(ClosingDelim::Paren),
            TokenType::RightSquare => Some(ClosingDelim::Square),
            TokenType::RightCurly => Some(ClosingDelim::Curly),
            TokenType::Else => Some(ClosingDelim::Else),
            TokenType::Comma => Some(ClosingDelim::Comma),
            TokenType::In => Some(ClosingDelim::In),
            _ => None,
        }
    }
}

pub fn lex_and_parse(input: &str) -> Vec<Node> {
    let tokens = lex(input);

    // let start = 600;
    // for (i, token) in tokens[start..start + 100].iter().enumerate() {
    //     println!("{}. {:?}", start + i, token);
    // }

    let mut tokens_iter = tokens.iter().enumerate().peekable();

    let parsed = parse(&mut tokens_iter);

    for node in &parsed {
        println!("{}", node);
    }

    parsed
}

pub fn parse(tokens: &mut Tokens) -> Vec<Node> {
    let mut exprs = Vec::new();

    while peek_token(tokens, true).is_some() {
        exprs.push(parse_expr(tokens, 0, false, true, true, &[]));

        match peek_delims(
            tokens,
            &[TokenType::NewLine, TokenType::SemiColon],
            &[],
            false,
        ) {
            Some((_, TokenType::NewLine | TokenType::SemiColon)) => {}
            Some(_) => panic!("Unexpected token"),
            _ => {}
        };

        next_token(tokens, false);
    }

    exprs
}

fn next_token<'a>(tokens: &'a mut Tokens, eat_lines: bool) -> Option<(usize, &'a Token)> {
    skip_whitespace(tokens, eat_lines);
    tokens.next()
}

fn peek_token<'a>(tokens: &'a mut Tokens, eat_lines: bool) -> Option<(usize, &'a Token)> {
    skip_whitespace(tokens, eat_lines);
    tokens.peek().map(|(i, token)| (*i, *token))
}

// expr_or_assign_or_help, expr_or_help, expr
fn parse_expr(
    tokens: &mut Tokens,
    min_binding_power: u8,
    eat_lines: bool,
    eq_assign: bool,
    binary_help: bool,
    context: &[ClosingDelim],
) -> Node {
    let (start, token) = match peek_token(tokens, true) {
        Some(x) => x,
        None => {
            panic!("Unexpected end of input");
            return Node::empty(NodeType::Expr);
        }
    };

    let token_type = token.token_type().clone();

    if !matches!(
        token_type,
        atoms!()
            | prefix_operators!()
            | TokenType::Help
            | TokenType::LeftCurly
            | TokenType::LeftParen
            | TokenType::Function
            | TokenType::BackSlash
            | TokenType::If
            | TokenType::For
            | TokenType::While
            | TokenType::Repeat
    ) {
        if is_closing(token.token_type(), eat_lines, context) {
            // step out - let the token close what it closes
            println!("Unexpected symbol: {:?}", (start, token.token_type()));
            todo!()
        } else if is_potentially_closing(token.token_type()) {
            // ignore the token
            todo!()
        } else if infix_binding_power(token.token_type()).is_some() {
            // Add an error token here, then parse the infix expression
            println!("Unexpected symbol: {:?}", (start, token.token_type()));
            todo!()
        } else {
            panic!("Unexpected symbol: {:?}", (start, token.token_type()))
        }
    }

    let (start, token) = next_token(tokens, eat_lines).unwrap();

    println!("{}, {:?}", start, token);

    let mut lhs = match token_type {
        // NUM_CONST, STR_CONST, NULL_CONST, PLACEHOLDER, SYMBOL,
        atoms!() => Node::ok(
            nodes::atom(token).expect("Node must be an atom"),
            Span::single(start),
            Vec::new(),
        ),
        // ( '-' | '+' | '!' | '~' ) expr
        // '?' expr_or_assign_or_help
        prefix_operators!() | TokenType::Help => {
            parse_prefix(tokens, context, &token_type, start, eat_lines)
        }
        // '{' exprlist '}'
        TokenType::LeftCurly => parse_braces(tokens, context, start),
        // '(' expr_or_assign_or_help ')'
        TokenType::LeftParen => parse_brackets(tokens, context, start),
        // FUNCTION '(' formlist ')' cr expr_or_assign_or_help %prec LOW
        // '\\' '(' formlist ')' cr expr_or_assign_or_help %prec LOW
        TokenType::Function | TokenType::BackSlash => parse_function(
            tokens,
            context,
            start,
            eat_lines,
            token_type == TokenType::Function,
        ),
        // IF ifcond expr_or_assign_or_help
        // IF ifcond expr_or_assign_or_help ELSE expr_or_assign_or_help
        TokenType::If => parse_if_statement(tokens, context, start, eat_lines),
        // FOR forcond expr_or_assign_or_help %prec FOR
        TokenType::For => parse_for_statement(tokens, context, start, eat_lines),
        // WHILE cond expr_or_assign_or_help
        TokenType::While => parse_while_statement(tokens, context, start, eat_lines),
        // REPEAT expr_or_assign_or_help
        TokenType::Repeat => parse_repeat_statement(tokens, context, start, eat_lines),
        _ => {
            panic!("Unexpected symbol: {:?}", (start, token.token_type()))
        }
    };

    loop {
        let (_, op) = match peek_token(tokens, eat_lines) {
            None => break,
            Some(token) => token,
        };

        if let Some((left_binding_power, right_binding_power)) =
            infix_binding_power(op.token_type())
        {
            if left_binding_power < min_binding_power
                || (op.token_type() == &TokenType::Help && !binary_help)
                || (op.token_type() == &TokenType::Equals && !eq_assign)
            {
                break;
            }

            let (op_index, op) = next_token(tokens, eat_lines).unwrap();
            let op = op.clone();

            lhs = match *op.token_type() {
                // expr '(' sublist ')'
                TokenType::LeftParen => parse_call(tokens, context, lhs, false, start, op_index),
                // expr '[' sublist ']'
                TokenType::LeftSquare => parse_call(tokens, context, lhs, true, start, op_index),
                // expr LBB sublist ']' ']'
                TokenType::LeftDoubleSquare => parse_index(tokens, context, lhs, start, op_index),
                // (SYMBOL | STR_CONST) (NS_GET | NS_GET_INT) (SYMBOL | STR_CONST)
                TokenType::DoubleColon | TokenType::TripleColon => parse_namespace(
                    tokens,
                    context,
                    lhs,
                    right_binding_power,
                    start,
                    op.token_type() == &TokenType::DoubleColon,
                ),
                // expr ('$' | '@') (SYMBOL | STR_CONST)
                TokenType::Dollar | TokenType::At => {
                    parse_extraction(tokens, context, lhs, right_binding_power, start)
                }
                // expr ('+' | '-' | ...etc) expr
                _ => parse_binary(
                    tokens,
                    context,
                    op,
                    lhs,
                    right_binding_power,
                    eat_lines,
                    start,
                ),
            }
        } else {
            break;
        }
    }

    lhs
}

fn get_delimiters(square: bool) -> (TokenType, TokenType) {
    if square {
        (TokenType::LeftSquare, TokenType::RightSquare)
    } else {
        (TokenType::LeftParen, TokenType::RightParen)
    }
}

fn parse_prefix(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    token_type: &TokenType,
    start: usize,
    eat_lines: bool,
) -> Node {
    let ((), right_binding_power) = prefix_binding_power(token_type)
        .expect("Every prefix operator must have a right binding power");
    let eq_assign = token_type == &TokenType::Help;
    let rhs = parse_expr(
        tokens,
        right_binding_power,
        eat_lines,
        eq_assign,
        false,
        context,
    );

    Node::ok(
        NodeType::PrefixCall,
        Span::new(start, rhs.end().unwrap_or(start)),
        vec![rhs],
    )
}

fn parse_braces(tokens: &mut Tokens, context: &[ClosingDelim], start: usize) -> Node {
    let context = &add_context(context, ClosingDelim::Curly);

    let mut exprs = Vec::new();

    loop {
        match peek_token(tokens, true) {
            Some((_, token)) if token.token_type() == &TokenType::RightCurly => {
                break;
            }
            Some((_, token)) if is_closing(token.token_type(), true, context) => {
                // TODO: Throw error
                panic!("Unexpected closing delimiter");
                break;
            }
            None => {
                // TODO: Throw error
                panic!("Unexpected closing delimiter");
                break;
            }
            Some(x) => {}
        }

        exprs.push(parse_expr(tokens, 0, false, true, true, context));

        if let Some((_, TokenType::NewLine | TokenType::SemiColon)) = peek_delims(
            tokens,
            &[
                TokenType::NewLine,
                TokenType::SemiColon,
                TokenType::RightCurly,
            ],
            context,
            false,
        ) {};
    }

    let delim_end = expect_delim(tokens, TokenType::RightCurly, context);

    let end = delim_end.unwrap_or(exprs.last().and_then(|x| x.end()).unwrap_or(start));

    Node::new(
        NodeType::Braces,
        Span::new(start, end),
        exprs,
        delim_end.is_none(),
    )
}

// '(' expr_or_assign_or_help ')'
fn parse_brackets(tokens: &mut Tokens, context: &[ClosingDelim], start: usize) -> Node {
    let context = &add_context(context, ClosingDelim::Paren);

    let inner = parse_expr(tokens, 0, true, true, true, context);

    let delim_end = expect_delim(tokens, TokenType::RightParen, context);

    let end = delim_end.unwrap_or(inner.end().unwrap_or(start));
    Node::new(
        NodeType::Parentheses,
        Span::new(start, end),
        vec![inner],
        delim_end.is_some(),
    )
}

fn parse_function(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    start: usize,
    eat_lines: bool,
    eat_lines_init: bool,
) -> Node {
    let args = parse_formlist(tokens, eat_lines_init, context);

    let body = parse_expr(tokens, 4, eat_lines, true, true, context);

    let end = body.end().unwrap_or(args.end().unwrap_or(start));
    Node::ok(NodeType::Function, Span::new(start, end), vec![args, body])
}

fn parse_if_statement(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    start: usize,
    eat_lines: bool,
) -> Node {
    let context = &add_context(context, ClosingDelim::Else);

    let condition = parse_condition(tokens, context);
    let (_, binding_power) = prefix_binding_power(&TokenType::If).unwrap();
    let expr = parse_expr(tokens, binding_power, eat_lines, true, true, context);
    skip_whitespace_after_if(tokens, eat_lines);

    let current_end = expr.end().unwrap_or(condition.end().unwrap_or(start));

    match peek_token(tokens, eat_lines) {
        Some((_, token)) if token.token_type() == &TokenType::Else => {
            next_token(tokens, eat_lines);
            let (_, binding_power) = prefix_binding_power(&TokenType::Else).unwrap();
            let else_expr = parse_expr(tokens, binding_power, eat_lines, true, true, context);

            Node::ok(
                NodeType::If,
                Span::new(start, else_expr.end().unwrap_or(current_end)),
                vec![condition, expr, else_expr],
            )
        }
        _ => Node::ok(
            NodeType::If,
            Span::new(start, current_end),
            vec![condition, expr],
        ),
    }
}

fn parse_for_statement(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    start: usize,
    eat_lines: bool,
) -> Node {
    let condition = parse_for_condition(
        tokens,
        &add_contexts(context, &[ClosingDelim::Paren, ClosingDelim::In]),
    );

    let (_, binding_power) = prefix_binding_power(&TokenType::For).unwrap();
    let expr = parse_expr(tokens, binding_power, eat_lines, true, true, context);

    let end = expr.end().unwrap_or(condition.end().unwrap_or(start));

    Node::ok(NodeType::For, Span::new(start, end), vec![condition, expr])
}

fn parse_while_statement(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    start: usize,
    eat_lines: bool,
) -> Node {
    let condition = parse_condition(tokens, context);

    let expr = parse_expr(tokens, 0, eat_lines, true, true, context);

    let end = expr.end().unwrap_or(condition.end().unwrap_or(start));

    Node::ok(
        NodeType::While,
        Span::new(start, end),
        vec![condition, expr],
    )
}

fn parse_repeat_statement(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    start: usize,
    eat_lines: bool,
) -> Node {
    let (_, binding_power) = prefix_binding_power(&TokenType::Repeat).unwrap();
    let expr = parse_expr(tokens, binding_power, eat_lines, true, true, context);

    let end = expr.end().unwrap_or(start);

    Node::ok(NodeType::Repeat, Span::new(start, end), vec![expr])
}

fn parse_call(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    lhs: Node,
    square: bool,
    start: usize,
    bracket_index: usize,
) -> Node {
    let node_type = if square {
        NodeType::Subset
    } else {
        NodeType::Call
    };

    let rhs = parse_sublist(tokens, context, square, bracket_index);

    let end = rhs.end().unwrap_or(bracket_index);

    Node::ok(node_type, Span::new(start, end), vec![lhs, rhs])
}

fn parse_index(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    lhs: Node,
    start: usize,
    token_index: usize,
) -> Node {
    let context = &add_context(context, ClosingDelim::Square);

    let rhs = parse_sublist(tokens, context, true, token_index);

    let second_square = expect_delim(tokens, TokenType::RightSquare, context);

    let end = second_square.unwrap_or(rhs.end().unwrap_or(start));

    Node::new(
        NodeType::Index,
        Span::new(start, end),
        vec![lhs, rhs],
        second_square.is_none(),
    )
}

fn parse_namespace(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    lhs: Node,
    right_binding_power: u8,
    start: usize,
    internal: bool,
) -> Node {
    if !matches!(
        lhs.node_type(),
        &NodeType::Symbol { .. } | &NodeType::LiteralString { .. }
    ) {
        // Turn the lhs into an error node
        todo!()
    };

    let rhs = parse_expr(tokens, right_binding_power, false, true, true, context);
    if !matches!(
        rhs.node_type(),
        &NodeType::Symbol { .. } | &NodeType::LiteralString { .. }
    ) {
        // Turn the rhs into an error node
        todo!()
    }

    Node::ok(
        NodeType::NameSpace { internal },
        Span::new(start, rhs.span().end()),
        vec![lhs, rhs],
    )
}

fn parse_extraction(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    lhs: Node,
    right_binding_power: u8,
    start: usize,
) -> Node {
    let rhs = parse_expr(tokens, right_binding_power, false, true, true, context);
    if !matches!(
        rhs.node_type(),
        &NodeType::Symbol { .. } | &NodeType::LiteralString { .. }
    ) {
        // Turn the lhs into an error node
        todo!()
    }

    Node::ok(
        NodeType::Extract,
        Span::new(start, rhs.span().end()),
        vec![lhs, rhs],
    )
}

fn parse_binary(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    op: Token,
    lhs: Node,
    right_binding_power: u8,
    eat_lines: bool,
    start: usize,
) -> Node {
    let rhs = parse_expr(tokens, right_binding_power, eat_lines, true, true, context);

    Node::ok(
        NodeType::Binary { op },
        Span::new(start, rhs.span().end()),
        vec![lhs, rhs],
    )
}

fn expect_delim(tokens: &mut Tokens, delim: TokenType, context: &[ClosingDelim]) -> Option<usize> {
    let mut end = None;
    #[allow(clippy::never_loop)]
    loop {
        match peek_token(tokens, true) {
            Some((_, token)) if token.token_type() == &delim => {
                end = Some(next_token(tokens, true).unwrap().0);
                break;
            }
            Some((i, token)) if is_closing(token.token_type(), true, context) => {
                // TODO: Throw error
                println!("Expected {:?}, got {:?}", delim, (i, token));
                panic!("Unexpected closing delimiter");
                break;
            }
            None => {
                // TODO: Throw error
                panic!("Unexpected closing delimiter");
                break;
            }
            Some(x) => {
                panic!("Unexpected token: {:?}", x);
            }
        }

        next_token(tokens, true).unwrap();
    }

    end
}

fn peek_delims(
    tokens: &mut Tokens,
    delims: &[TokenType],
    context: &[ClosingDelim],
    eat_lines: bool,
) -> Option<(usize, TokenType)> {
    #[allow(clippy::never_loop)]
    loop {
        match peek_token(tokens, eat_lines) {
            Some((i, token)) if delims.contains(token.token_type()) => {
                return Some((i, token.token_type().clone()));
            }
            Some((_, token)) if is_closing(token.token_type(), true, context) => {
                // TODO: Throw error
                panic!("Unexpected closing delimiter");
                return None;
            }
            None => {
                // TODO: Throw error
                panic!("Unexpected end of input");
                return None;
            }
            Some(x) => {
                panic!("Unexpected token: {:?}", x);
                next_token(tokens, true);
            }
        }
    }
}

fn parse_condition(tokens: &mut Tokens, context: &[ClosingDelim]) -> Node {
    let context = &add_context(context, ClosingDelim::Paren);

    let (start, _) = match peek_token(tokens, true) {
        Some((_, token)) if token.token_type() == &TokenType::LeftParen => {
            next_token(tokens, true).unwrap()
        }
        x => {
            return {
                panic!("Expected '(', got {:?}", x);
                Node::empty(NodeType::FormList)
            }
        }
    };

    let cond = parse_expr(tokens, 0, true, false, true, context);

    let delim_end = expect_delim(tokens, TokenType::RightParen, context);

    let end = delim_end.unwrap_or(cond.end().unwrap_or(start));

    Node::new(
        NodeType::Condition,
        Span::new(start, end),
        vec![cond],
        delim_end.is_none(),
    )
}

// forcond :    '(' SYMBOL IN expr_or_help ')'
fn parse_for_condition(tokens: &mut Tokens, context: &[ClosingDelim]) -> Node {
    let context = &add_context(context, ClosingDelim::Paren);

    let (start, _) = match peek_token(tokens, true) {
        Some((_, token)) if token.token_type() == &TokenType::LeftParen => {
            next_token(tokens, true).unwrap()
        }
        x => {
            panic!("Expected '(', got {:?}", x);
            return Node::empty(NodeType::FormList);
        }
    };

    let lhs = parse_expr(tokens, 0, true, false, true, context);

    let in_op = expect_delim(tokens, TokenType::In, context);

    let rhs = parse_expr(tokens, 0, true, false, true, context);

    let delim_end = expect_delim(tokens, TokenType::RightParen, context);

    let end = delim_end.unwrap_or(
        rhs.end()
            .unwrap_or(in_op.unwrap_or(lhs.end().unwrap_or(start))),
    );

    Node::new(
        NodeType::ForCondition,
        Span::new(start, end),
        vec![lhs, rhs],
        delim_end.is_none() || in_op.is_none(),
    )
}

fn skip_whitespace(tokens: &mut Tokens, eat_lines: bool) {
    while tokens
        .next_if(|(_, token)| {
            token.token_type() == &TokenType::WhiteSpace
                || token.token_type() == &TokenType::Comment
                || (eat_lines && token.token_type() == &TokenType::NewLine)
        })
        .is_some()
    {}
}

fn skip_whitespace_after_if(tokens: &mut Tokens, eat_lines: bool) {
    if eat_lines {
        skip_whitespace(tokens, true);
        return;
    }

    let next_token = tokens.clone().find(|(_, token)| {
        !matches!(
            token.token_type(),
            &TokenType::WhiteSpace | &TokenType::NewLine | &TokenType::Comment
        )
    });

    if next_token.is_some_and(|(_, token)| token.token_type() == &TokenType::Else) {
        skip_whitespace(tokens, true)
    } else {
        skip_whitespace(tokens, false)
    }
}

fn parse_formlist(tokens: &mut Tokens, eat_lines_init: bool, context: &[ClosingDelim]) -> Node {
    let (start, _) = match peek_token(tokens, eat_lines_init) {
        Some((_, token)) if token.token_type() == &TokenType::LeftParen => {
            next_token(tokens, true).unwrap()
        }
        x => {
            panic!("Expected '(', got {:?}", x);
            return Node::empty(NodeType::FormList);
        }
    };

    parse_list(
        tokens,
        NodeType::FormList,
        false,
        start,
        context,
        parse_formlist_item,
    )
}

fn parse_sublist(
    tokens: &mut Tokens,
    context: &[ClosingDelim],
    square: bool,
    start: usize,
) -> Node {
    parse_list(
        tokens,
        NodeType::SubList,
        square,
        start,
        context,
        parse_sublist_item,
    )
}

fn parse_list(
    tokens: &mut Tokens,
    node_type: NodeType,
    square: bool,
    start: usize,
    context: &[ClosingDelim],
    parse_item: impl Fn(&mut Tokens, &[ClosingDelim]) -> Option<Node>,
) -> Node {
    let context = &add_contexts(
        context,
        &[
            if square {
                ClosingDelim::Square
            } else {
                ClosingDelim::Paren
            },
            ClosingDelim::Comma,
        ],
    );

    let (_, rhs) = get_delimiters(square);

    let mut args = Vec::new();
    let delims = &[rhs.clone(), TokenType::Comma];

    loop {
        if let Some(item) = parse_item(tokens, context) {
            args.push(item);
        }

        let delim = peek_delims(tokens, delims, context, true);

        match delim {
            Some((_, TokenType::Comma)) => {}
            Some((_, token_type)) if is_closing(&token_type, true, context) => break,
            Some(x) => {
                println!("{:?}", x);
                panic!("Expected comma or closing bracket");
            }
            None => break,
        };

        next_token(tokens, true);
    }

    let final_delim = expect_delim(tokens, rhs, context);

    let end = final_delim.unwrap_or(args.last().and_then(|x| x.end()).unwrap_or(start));

    Node::new(
        node_type,
        Span::new(start, end),
        args,
        final_delim.is_none(),
    )
}

// sublist  :   sub
//          |   sublist cr ',' sub      { $$ = xxsublist2($1,$4); }
//          ;
//
// sub  :
//      |   expr_or_help
//      |   SYMBOL EQ_ASSIGN
//      |   SYMBOL EQ_ASSIGN expr_or_help
//      |   STR_CONST EQ_ASSIGN
//      |   STR_CONST EQ_ASSIGN expr_or_help
//      |   NULL_CONST EQ_ASSIGN
//      |   NULL_CONST EQ_ASSIGN expr_or_help
//      ;
fn parse_sublist_item(tokens: &mut Tokens, context: &[ClosingDelim]) -> Option<Node> {
    if peek_token(tokens, true).is_some_and(|(_, token)| {
        token.token_type() == &TokenType::Comma || is_closing(token.token_type(), true, context)
    }) {
        return None;
    }

    let lhs = parse_expr(tokens, 0, true, false, true, context);

    let (equals_index, next) = peek_token(tokens, true).unwrap();

    if next.token_type() != &TokenType::Equals && next.token_type() != &TokenType::ColonEquals {
        return Some(Node::wraps(NodeType::SubListItem, lhs));
    } else {
        if !matches!(
            lhs.node_type(),
            &NodeType::Symbol { .. } | &NodeType::LiteralString { .. } | &NodeType::Null
        ) {
            // TODO: Error and continue
            panic!("Unexpected closing delimiter");
        }

        next_token(tokens, true);
    }

    let start = lhs.start().unwrap_or(equals_index);
    if peek_token(tokens, true)
        .is_some_and(|(_, token)| is_closing(token.token_type(), true, context))
    {
        return Some(Node::ok(
            NodeType::SubListItem,
            Span::new(start, equals_index),
            vec![lhs],
        ));
    }

    let rhs = parse_expr(tokens, 0, true, false, true, context);
    let end = rhs.end().unwrap_or(equals_index);
    Some(Node::ok(
        NodeType::SubListItem,
        Span::new(start, end),
        vec![lhs, rhs],
    ))
}

// formlist:
//         |    SYMBOL
//         |    SYMBOL EQ_ASSIGN expr_or_help
//         |    formlist ',' SYMBOL
//         |    formlist ',' SYMBOL EQ_ASSIGN expr_or_help
fn parse_formlist_item(tokens: &mut Tokens, context: &[ClosingDelim]) -> Option<Node> {
    if peek_token(tokens, true).is_some_and(|(_, token)| {
        token.token_type() == &TokenType::Comma || is_closing(token.token_type(), true, context)
    }) {
        return None;
    }

    let lhs = parse_expr(tokens, 0, true, false, true, context);

    if !matches!(lhs.node_type(), NodeType::Symbol { .. }) {
        // TODO: Throw error
        panic!("Unexpected closing delimiter");
    }

    let (equals_index, next) = peek_token(tokens, true).unwrap();

    if next.token_type() != &TokenType::Equals && next.token_type() != &TokenType::ColonEquals {
        return Some(Node::wraps(NodeType::FormListItem, lhs));
    } else {
        next_token(tokens, true);
    }

    let rhs = parse_expr(tokens, 0, true, true, true, context);

    let start = lhs.start().unwrap_or(equals_index);
    let end = rhs.end().unwrap_or(equals_index);
    Some(Node::ok(
        NodeType::FormListItem,
        Span::new(start, end),
        vec![lhs, rhs],
    ))
}

// Edited version of R precedence table
//
// Associativity  Names                   Binding power       Notes
// %left          '?'                     (1, 2) | ((), 2)
// %left          LOW WHILE FOR REPEAT    (3, 4)              Low is after a functon body
// %right         IF                      (6, 5)
// %left          ELSE                    (7, 8)
// %right         LEFT_ASSIGN             (12, 11)            <-, <<-, :=
// %right         EQ_ASSIGN               (10, 9)             =
// %left          RIGHT_ASSIGN            (13, 14)            ->, =>>
// %left          '~' TILDE               (15, 16)
// %left          OR OR2                  (17, 18)
// %left          AND AND2                (19, 20)
// %left          UNOT NOT                (21, 22)
// %nonassoc      GT GE LT LE EQ NE       (23, 23)
// %left          '+' '-'                 (24, 25)
// %left          '*' '/'                 (26, 27)
// %left          SPECIAL PIPE            (28, 29)            %%, |>
// %left          PIPEBIND                (30, 31)
// %left          ':'                     (32, 33)
// %left          UMINUS UPLUS            (34, 35)            Unary
// %right         '^'                     (37, 36)
// %left          '$' '@'                 (38, 39)
// %left          NS_GET NS_GET_INT       (40, 41)            ::, :::
// %nonassoc      '(' '[' LBB             (42, 42)            [[
//
// There are some operations that break these rules due to how the grammar is written.
// For example, = and <- are swapped in precedence.

// Determines the order of operations for infix operators
//
// If an operation has a larger binding power on its right than on its left, it
// is left associative. For example, if + has power (1, 2):
// 1   +   2   +   3
//  (1) (2) (1) (2)
// = (1 + 2) + 3
//
// If an operation is not associative, use the same binding power to cause an error if
// two are used in a row.
//
// https://rdrr.io/r/base/Syntax.html
fn infix_binding_power(token: &TokenType) -> Option<(u8, u8)> {
    match *token {
        TokenType::Help => Some((1, 2)),
        TokenType::Equals => Some((10, 9)),
        TokenType::LeftAssign | TokenType::DoubleLeftAssign => Some((12, 11)),
        TokenType::RightAssign | TokenType::DoubleRightAssign => Some((13, 14)),
        TokenType::Tilde => Some((15, 16)),
        TokenType::Or | TokenType::DoubleOr => Some((17, 18)),
        TokenType::And | TokenType::DoubleAnd => Some((19, 20)),
        TokenType::LessThan
        | TokenType::GreaterThan
        | TokenType::LessThanEquals
        | TokenType::GreaterThanEquals
        | TokenType::DoubleEquals
        | TokenType::NotEquals => Some((23, 23)),
        TokenType::Plus | TokenType::Minus => Some((24, 25)),
        TokenType::Asterisk | TokenType::Slash => Some((26, 27)),
        TokenType::Infix | TokenType::Pipe => Some((28, 29)),
        TokenType::PipeBind => Some((30, 31)),
        TokenType::Colon => Some((32, 33)),
        TokenType::Caret | TokenType::DoubleAsterisk => Some((37, 36)),
        TokenType::LeftParen => Some((31, 32)),
        TokenType::LeftSquare | TokenType::LeftDoubleSquare => Some((33, 34)),
        TokenType::Dollar | TokenType::At => Some((35, 36)),
        TokenType::DoubleColon | TokenType::TripleColon => Some((37, 38)),
        _ => None,
    }
}

fn prefix_binding_power(token_type: &TokenType) -> Option<((), u8)> {
    match *token_type {
        TokenType::Help => Some(((), 2)),
        TokenType::While | TokenType::For | TokenType::Repeat => Some(((), 3)),
        TokenType::If => Some(((), 5)),
        TokenType::Else => Some(((), 8)),
        TokenType::Tilde => Some(((), 10)),
        TokenType::Plus | TokenType::Minus => Some(((), 24)),
        TokenType::Not => Some(((), 16)),
        _ => None,
    }
}

fn is_closing(token_type: &TokenType, eat_lines: bool, context: &[ClosingDelim]) -> bool {
    let closes = ClosingDelim::closes(token_type);

    match token_type {
        TokenType::SemiColon => true,
        TokenType::NewLine if !eat_lines => true,
        _ if closes.is_some_and(|x| context.contains(&x)) => true,
        _ => false,
    }
}

fn is_potentially_closing(token_type: &TokenType) -> bool {
    ClosingDelim::closes(token_type).is_some()
}

fn add_context(context: &[ClosingDelim], value: ClosingDelim) -> Vec<ClosingDelim> {
    let mut vec = context.to_vec();
    vec.push(value);
    vec
}

fn add_contexts(context: &[ClosingDelim], values: &[ClosingDelim]) -> Vec<ClosingDelim> {
    let mut vec = context.to_vec();
    vec.extend_from_slice(values);
    vec
}
