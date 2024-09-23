use core::panic;
use std::iter::{Enumerate, Peekable};
use std::slice::Iter;

use ropey::Rope;

use crate::grammar::{Span, Token, TokenType};

use crate::lexer::lex;
use crate::nodes::{self, Node, NodeType};

type Tokens<'a> = Peekable<Enumerate<Iter<'a, Token>>>;

/// Errors that occur during parsing. `span` refers to the set of tokens that caused the error. If
/// it is `None`, then the error is at the end of the input (i.e. an unclosed delimiter).
pub struct ParseError {
    message: &'static str,
    span: Option<Span>,
}

impl ParseError {
    pub fn new(message: &'static str, span: Option<Span>) -> Self {
        Self { message, span }
    }

    pub fn single(message: &'static str, span: usize) -> Self {
        Self {
            message,
            span: Some(Span::single(span)),
        }
    }

    pub fn at_end(message: &'static str) -> Self {
        Self {
            message,
            span: None,
        }
    }
}

pub fn lex_and_parse(input: &Rope) -> Vec<Node> {
    let (tokens, errors) = lex(input);

    for error in errors {
        println!("{}: {}", error.index, error.message);
    }

    // let start = 600;
    // for (i, token) in tokens[start..start + 100].iter().enumerate() {
    //     println!("{}. {:?}", start + i, token);
    // }

    let mut tokens_iter = tokens.iter().enumerate().peekable();

    let (parsed, errors) = parse(&mut tokens_iter);

    if errors.is_empty() {
        for node in &parsed {
            println!("{}", node);
        }
    } else {
        for error in errors {
            match error.span {
                Some(x) => {
                    println!("({}, {}): {}", x.start(), x.end(), error.message);
                }
                None => {
                    println!("{}", error.message);
                }
            }
        }
    }

    parsed
}

/// Given a list of tokens produced by lexing, parse them into a syntax tree.
///
/// The parser is a hand written Pratt parser, based on the grammar defined in the [source code of
/// R](https://github.com/wch/r-source/blob/trunk/src/main/gram.y).
///
/// This function parses the `prog` item of the grammar, defined as:
///
/// END_OF_INPUT | '\n' | expr_or_assign_or_help '\n' | expr_or_assign_or_help ';' | error
pub fn parse(tokens: &mut Tokens) -> (Vec<Node>, Vec<ParseError>) {
    let mut exprs = Vec::new();
    let mut errors = Vec::new();

    while peek_token(tokens, true).is_some() {
        exprs.push(parse_expr(tokens, &mut errors, 0, false, true, true, &[]));

        // Works since this does not produce an error if there is no next token
        peek_delims(
            tokens,
            &mut errors,
            &[TokenType::NewLine, TokenType::SemiColon],
            &[],
            false,
            false,
            "Expected a newline or ';'",
        );

        // Consume the token, if there is one
        next_token(tokens, false);
    }

    (exprs, errors)
}

/// Get the next token, skipping whitespace. If `eat_lines` is true, then skip newlines as well.
fn next_token<'a>(tokens: &'a mut Tokens, eat_lines: bool) -> Option<(usize, &'a Token)> {
    skip_whitespace(tokens, eat_lines);
    tokens.next()
}

fn peek_token<'a>(tokens: &'a mut Tokens, eat_lines: bool) -> Option<(usize, &'a Token)> {
    skip_whitespace(tokens, eat_lines);
    tokens.peek().map(|(i, token)| (*i, *token))
}

// expr_or_assign_or_help, expr_or_help, expr
/// The heart of the parser. Parses any non-empty expression. R programs are made up of
/// expressions, so the parsing process involves repeatedly calling this function.
///
/// Parses the following nodes: `expr_or_assign_or_help`, `expr_or_help`, `expr`, defined as:
///
/// expr_or_assign_or_help :
///     expr
///     | expr_or_assign_or_help EQ_ASSIGN expr_or_assign_or_help
///     | expr_or_assign_or_help '?' expr_or_assign_or_help
///
/// expr_or_help :
///     expr | expr_or_help '?' expr_or_help
///
/// expr: NUM_CONST
///     | STR_CONST
///     | NULL_CONST
///     | PLACEHOLDER
///     | SYMBOL
///     ...
///     | NEXT
///     | BREAK
///
/// `min_binding_power` is used in [parse_infix()]
///
/// `eat_lines` controls whether newlines can be skipped.
///
/// `eq_assign` defines whether `=` is allowed to be used as an assignment operator.
///
/// `binary_help` defines whether `?` is allowed to be used as a binary operator (e.g.
/// `package?utils`).
///
/// `context` stores the delimiters that can close parent nodes of the tree.
fn parse_expr(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    min_binding_power: u8,
    eat_lines: bool,
    eq_assign: bool,
    binary_help: bool,
    context: &[TokenType],
) -> Node {
    let (start, token) = match peek_token(tokens, true) {
        Some(x) => x,
        None => {
            errors.push(ParseError::at_end("Expected expression"));
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
        if is_closing(token.token_type(), context) {
            // step out - let the token close what it closes
            errors.push(ParseError::single("Expected expression", start));
            return Node::empty(NodeType::Expr);
        } else if infix_binding_power(token.token_type()).is_some() {
            // Parse the infix expression, using an empty node as the lhs
            errors.push(ParseError::single("Expected expression", start));

            let lhs = Node::empty(NodeType::Expr);
            return parse_infix(
                tokens,
                errors,
                lhs,
                start,
                min_binding_power,
                eat_lines,
                binary_help,
                eq_assign,
                context,
            );
        } else {
            // Skip the token
            errors.push(ParseError::single("Unexpected token", start));
            next_token(tokens, true).expect("The token has already been peeked");

            return parse_expr(
                tokens,
                errors,
                min_binding_power,
                eat_lines,
                eq_assign,
                binary_help,
                context,
            );
        }
    }

    let (start, token) = next_token(tokens, eat_lines).expect("The token has already been peeked");

    let lhs = match token_type {
        // NUM_CONST, STR_CONST, NULL_CONST, PLACEHOLDER, SYMBOL,
        atoms!() => Node::ok(
            nodes::atom(token).expect("Node must be an atom"),
            Span::single(start),
            Vec::new(),
        ),
        prefix_operators!() | TokenType::Help => {
            parse_prefix(tokens, errors, context, &token_type, start, eat_lines)
        }
        // '{' exprlist '}'
        TokenType::LeftCurly => parse_braces(tokens, errors, context, start),
        // '(' expr_or_assign_or_help ')'
        TokenType::LeftParen => parse_brackets(tokens, errors, context, start),
        // FUNCTION '(' formlist ')' cr expr_or_assign_or_help %prec LOW
        // '\\' '(' formlist ')' cr expr_or_assign_or_help %prec LOW
        TokenType::Function | TokenType::BackSlash => parse_function(
            tokens,
            errors,
            context,
            start,
            eat_lines,
            token_type == TokenType::Function,
        ),
        // IF ifcond expr_or_assign_or_help
        // IF ifcond expr_or_assign_or_help ELSE expr_or_assign_or_help
        TokenType::If => parse_if_statement(tokens, errors, context, start, eat_lines),
        // FOR forcond expr_or_assign_or_help %prec FOR
        TokenType::For => parse_for_statement(tokens, errors, context, start, eat_lines),
        // WHILE cond expr_or_assign_or_help
        TokenType::While => parse_while_statement(tokens, errors, context, start, eat_lines),
        // REPEAT expr_or_assign_or_help
        TokenType::Repeat => parse_repeat_statement(tokens, errors, context, start, eat_lines),
        _ => {
            panic!("Unexpected symbol: {:?}", (start, token.token_type()))
        }
    };

    parse_infix(
        tokens,
        errors,
        lhs,
        start,
        min_binding_power,
        eat_lines,
        binary_help,
        eq_assign,
        context,
    )
}

/// Given a parsed expression, parse any infix expressions following it.
///
/// `min_binding_power` is the binding power of the infix or prefix operator that
/// precedes the expression currently being parsed (see [prefix_binding_power()]
/// and [infix_binding_power()]. If there is no such operator, then this is 0.
///
/// The function repeatedly parses infix operators until it encounters the end
/// of the expression or another operator with a lower binding power.
#[allow(clippy::too_many_arguments)]
fn parse_infix(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    mut lhs: Node,
    start: usize,
    min_binding_power: u8,
    eat_lines: bool,
    binary_help: bool,
    eq_assign: bool,
    context: &[TokenType],
) -> Node {
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

            let (op_index, op) =
                next_token(tokens, eat_lines).expect("The token has already been peeked");
            let op = op.clone();

            lhs = match *op.token_type() {
                // expr '(' sublist ')'
                TokenType::LeftParen => {
                    parse_call(tokens, errors, context, lhs, false, start, op_index)
                }
                // expr '[' sublist ']'
                TokenType::LeftSquare => {
                    parse_call(tokens, errors, context, lhs, true, start, op_index)
                }
                // expr LBB sublist ']' ']'
                TokenType::LeftDoubleSquare => {
                    parse_index(tokens, errors, context, lhs, start, op_index)
                }
                // (SYMBOL | STR_CONST) (NS_GET | NS_GET_INT) (SYMBOL | STR_CONST)
                TokenType::DoubleColon | TokenType::TripleColon => parse_namespace(
                    tokens,
                    errors,
                    context,
                    lhs,
                    right_binding_power,
                    start,
                    op.token_type() == &TokenType::DoubleColon,
                ),
                // expr ('$' | '@') (SYMBOL | STR_CONST)
                TokenType::Dollar | TokenType::At => {
                    parse_extraction(tokens, errors, context, lhs, right_binding_power, start)
                }
                // expr ('+' | '-' | ...etc) expr
                _ => parse_binary(
                    tokens,
                    errors,
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

/// Parse a prefix operator (e.g. `- 1`)
///
/// Parses the following expressions:
/// ( '-' | '+' | '!' | '~' ) expr
/// '?' expr_or_assign_or_help
fn parse_prefix(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    token_type: &TokenType,
    start: usize,
    eat_lines: bool,
) -> Node {
    let ((), right_binding_power) = prefix_binding_power(token_type)
        .expect("Every prefix operator must have a right binding power");
    let eq_assign = token_type == &TokenType::Help;
    let binary_help = eq_assign;
    let rhs = parse_expr(
        tokens,
        errors,
        right_binding_power,
        eat_lines,
        eq_assign,
        binary_help,
        context,
    );

    Node::ok(
        NodeType::PrefixCall,
        Span::new(start, rhs.end().unwrap_or(start)),
        vec![rhs],
    )
}

/// Parse a set of expressions in curly braces (`{}`):
///
/// '{' exprlist '}'
///
/// exprlist: expr_or_assign_or_help
///     | exprlist ';' expr_or_assign_or_help
///     | exprlist ';'
///     | exprlist '\n' expr_or_assign_or_help
///     | exprlist '\n'
///
fn parse_braces(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    start: usize,
) -> Node {
    let context = &add_context(context, TokenType::RightCurly);

    let mut exprs = Vec::new();

    loop {
        match peek_token(tokens, true) {
            Some((_, token)) if token.token_type() == &TokenType::RightCurly => {
                break;
            }
            Some((i, token)) if is_closing(token.token_type(), context) => {
                // TODO: Throw error
                errors.push(ParseError::single("Expected '}'", i));
                break;
            }
            None => {
                // TODO: Throw error
                errors.push(ParseError::at_end("Expected '}'"));
                break;
            }
            Some(_) => {}
        }

        exprs.push(parse_expr(tokens, errors, 0, false, true, true, context));

        if let Some((_, TokenType::NewLine | TokenType::SemiColon)) = peek_delims(
            tokens,
            errors,
            &[
                TokenType::NewLine,
                TokenType::SemiColon,
                TokenType::RightCurly,
            ],
            context,
            false,
            true,
            "Expected a newline, '}' or ';'",
        ) {};
    }

    let delim_end = expect_delim(
        tokens,
        errors,
        TokenType::RightCurly,
        context,
        "Expected '}'",
    );

    let end = delim_end.unwrap_or(exprs.last().and_then(|x| x.end()).unwrap_or(start));

    Node::new(
        NodeType::Braces,
        Span::new(start, end),
        exprs,
        delim_end.is_none(),
    )
}

/// Parse an expression in brackets:
/// '(' expr_or_assign_or_help ')'
fn parse_brackets(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    start: usize,
) -> Node {
    let context = &add_context(context, TokenType::RightParen);

    let inner = parse_expr(tokens, errors, 0, true, true, true, context);

    let delim_end = expect_delim(
        tokens,
        errors,
        TokenType::RightParen,
        context,
        "Expected ')",
    );

    let end = delim_end.unwrap_or(inner.end().unwrap_or(start));
    Node::new(
        NodeType::Parentheses,
        Span::new(start, end),
        vec![inner],
        delim_end.is_some(),
    )
}

/// Parses a function definition.
///
/// Parses the following expressions:
///
/// FUNCTION '(' formlist ')' expr_or_assign_or_help
/// '\\' '(' formlist ')' expr_or_assign_or_help
///
/// Note that lambda functions do not allow any newlines before the formlist starts, so
/// `eat_lines_init` controls whether to skip newlines before parsing the formlist.
fn parse_function(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    start: usize,
    eat_lines: bool,
    eat_lines_init: bool,
) -> Node {
    let args = parse_formlist(tokens, errors, eat_lines_init, context);

    let body = parse_expr(tokens, errors, 4, eat_lines, true, true, context);

    let end = body.end().unwrap_or(args.end().unwrap_or(start));
    Node::ok(NodeType::Function, Span::new(start, end), vec![args, body])
}

/// Parses an if statement, defined as the following expressions:
///
/// IF ifcond expr_or_assign_or_help
/// IF ifcond expr_or_assign_or_help ELSE expr_or_assign_or_help
///
/// Note that newlines after the condition need to be treated carefully, see
/// [skip_whitespace_after_if()].
fn parse_if_statement(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    start: usize,
    eat_lines: bool,
) -> Node {
    let context = &add_context(context, TokenType::Else);

    let condition = parse_condition(tokens, errors, context);
    let (_, binding_power) = prefix_binding_power(&TokenType::If).unwrap();
    let expr = parse_expr(
        tokens,
        errors,
        binding_power,
        eat_lines,
        true,
        true,
        context,
    );
    skip_whitespace_after_if(tokens, eat_lines);

    let current_end = expr.end().unwrap_or(condition.end().unwrap_or(start));

    match peek_token(tokens, eat_lines) {
        Some((_, token)) if token.token_type() == &TokenType::Else => {
            next_token(tokens, eat_lines).expect("The token has already been peeked");
            let (_, binding_power) = prefix_binding_power(&TokenType::Else).unwrap();
            let else_expr = parse_expr(
                tokens,
                errors,
                binding_power,
                eat_lines,
                true,
                true,
                context,
            );

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

/// Parse a for statement, defined as:
///
/// FOR forcond expr_or_assign_or_help
fn parse_for_statement(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    start: usize,
    eat_lines: bool,
) -> Node {
    let condition = parse_for_condition(
        tokens,
        errors,
        &add_contexts(context, &[TokenType::RightParen, TokenType::In]),
    );

    let (_, binding_power) = prefix_binding_power(&TokenType::For).unwrap();
    let expr = parse_expr(
        tokens,
        errors,
        binding_power,
        eat_lines,
        true,
        true,
        context,
    );

    let end = expr.end().unwrap_or(condition.end().unwrap_or(start));

    Node::ok(NodeType::For, Span::new(start, end), vec![condition, expr])
}

/// Parse a while statement, defined as:
///
/// WHILE whilecond expr_or_assign_or_help
fn parse_while_statement(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    start: usize,
    eat_lines: bool,
) -> Node {
    let condition = parse_condition(tokens, errors, context);

    let expr = parse_expr(tokens, errors, 0, eat_lines, true, true, context);

    let end = expr.end().unwrap_or(condition.end().unwrap_or(start));

    Node::ok(
        NodeType::While,
        Span::new(start, end),
        vec![condition, expr],
    )
}

/// Parse a repeat statement, defined as:
///
/// REPEAT expr_or_assign_or_help
fn parse_repeat_statement(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    start: usize,
    eat_lines: bool,
) -> Node {
    let (_, binding_power) = prefix_binding_power(&TokenType::Repeat).unwrap();
    let expr = parse_expr(
        tokens,
        errors,
        binding_power,
        eat_lines,
        true,
        true,
        context,
    );

    let end = expr.end().unwrap_or(start);

    Node::ok(NodeType::Repeat, Span::new(start, end), vec![expr])
}

fn parse_call(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
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

    let rhs = parse_sublist(tokens, errors, context, square, bracket_index);

    let end = rhs.end().unwrap_or(bracket_index);

    Node::ok(node_type, Span::new(start, end), vec![lhs, rhs])
}

/// Parse an indexing (`[[]]`) operation, defined as:
///
/// expr LBB sublist ']' ']'
fn parse_index(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    lhs: Node,
    start: usize,
    token_index: usize,
) -> Node {
    let context = &add_context(context, TokenType::RightSquare);

    let rhs = parse_sublist(tokens, errors, context, true, token_index);

    let second_square = expect_delim(
        tokens,
        errors,
        TokenType::RightSquare,
        context,
        "Expected ']",
    );

    let end = second_square.unwrap_or(rhs.end().unwrap_or(start));

    Node::new(
        NodeType::Index,
        Span::new(start, end),
        vec![lhs, rhs],
        second_square.is_none(),
    )
}

/// Parse a namespacing operation (`::` or `:::`) defined as the following expressions:
///
/// SYMBOL NS_GET SYMBOL
/// SYMBOL NS_GET STR_CONST
/// STR_CONST NS_GET SYMBOL
/// STR_CONST NS_GET STR_CONST
/// SYMBOL NS_GET_INT SYMBOL
/// SYMBOL NS_GET_INT STR_CONST
/// STR_CONST NS_GET_INT SYMBOL
/// STR_CONST NS_GET_INT STR_CONST
fn parse_namespace(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    lhs: Node,
    right_binding_power: u8,
    start: usize,
    internal: bool,
) -> Node {
    let lhs = if !matches!(
        lhs.node_type(),
        &NodeType::Symbol { .. } | &NodeType::LiteralString { .. }
    ) {
        // Turn the lhs into an error node
        errors.push(ParseError::new(
            "Expected a symbol or string",
            Some(lhs.span().clone()),
        ));
        lhs.as_error()
    } else {
        lhs
    };

    let rhs = parse_expr(
        tokens,
        errors,
        right_binding_power,
        false,
        true,
        true,
        context,
    );

    let rhs = if !matches!(
        rhs.node_type(),
        &NodeType::Symbol { .. } | &NodeType::LiteralString { .. }
    ) {
        errors.push(ParseError::new(
            "Expected a symbol or string",
            Some(lhs.span().clone()),
        ));
        rhs.as_error()
    } else {
        rhs
    };

    Node::ok(
        NodeType::NameSpace { internal },
        Span::new(start, rhs.span().end()),
        vec![lhs, rhs],
    )
}

/// Parse an extraction expression (`$` or `@`), defined as the following expressions:
///
/// expr '$' SYMBOL
/// expr '$' STR_CONST
/// expr '@' SYMBOL
/// expr '@' STR_CONST
fn parse_extraction(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    lhs: Node,
    right_binding_power: u8,
    start: usize,
) -> Node {
    let rhs = parse_expr(
        tokens,
        errors,
        right_binding_power,
        false,
        true,
        true,
        context,
    );

    let rhs = if !matches!(
        rhs.node_type(),
        &NodeType::Symbol { .. } | &NodeType::LiteralString { .. }
    ) {
        // Turn the lhs into an error node
        errors.push(ParseError::new(
            "Expected a symbol or string",
            Some(rhs.span().clone()),
        ));
        rhs.as_error()
    } else {
        rhs
    };

    Node::ok(
        NodeType::Extract,
        Span::new(start, rhs.span().end()),
        vec![lhs, rhs],
    )
}

/// Parse an expression that uses a binary/infix operator, defined as the following expressions:
///
/// expr ':'  expr
/// expr '+'  expr
/// expr '-' expr
/// expr '*' expr
/// expr '/' expr
/// expr '^' expr
/// expr SPECIAL expr
/// expr '~' expr
/// expr LT expr
/// expr LE expr
/// expr EQ expr
/// expr NE expr
/// expr GE expr
/// expr GT expr
/// expr AND expr
/// expr OR expr
/// expr AND2 expr
/// expr OR2 expr
/// expr PIPE expr
/// expr PIPEBIND expr
/// expr LEFT_ASSIGN expr
/// expr RIGHT_ASSIGN expr
#[allow(clippy::too_many_arguments)]
fn parse_binary(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    op: Token,
    lhs: Node,
    right_binding_power: u8,
    eat_lines: bool,
    start: usize,
) -> Node {
    let rhs = parse_expr(
        tokens,
        errors,
        right_binding_power,
        eat_lines,
        true,
        true,
        context,
    );

    Node::ok(
        NodeType::Binary { op },
        Span::new(start, rhs.span().end()),
        vec![lhs, rhs],
    )
}

/// Parse until a delimiter is found, and return the index of the delimiter
///
/// Returns [None] if the end of the file is reached, or if another delimiter
/// that closes a parent context is found (e.g. `({)`).
///
/// Creates an error every time for every unexpected token that is found.
fn expect_delim(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    delim: TokenType,
    context: &[TokenType],
    error_message: &'static str,
) -> Option<usize> {
    let mut end = None;
    loop {
        match peek_token(tokens, true) {
            Some((_, token)) if token.token_type() == &delim => {
                end = Some(
                    next_token(tokens, true)
                        .expect("The token has already been peeked")
                        .0,
                );
                break;
            }
            Some((i, token)) if is_closing(token.token_type(), context) => {
                errors.push(ParseError::single(error_message, i));
                break;
            }
            None => {
                // TODO: Throw error
                errors.push(ParseError::at_end(error_message));
                break;
            }
            Some((i, _)) => {
                errors.push(ParseError::single(error_message, i));
            }
        }

        next_token(tokens, true).expect("The token has already been peeked");
    }

    end
}

/// A version of [expect_delim()] that looks for a set of delimiters, returning the type that is
/// found.
///
/// `error_on_eof` controls whether an error is created if the end of the
/// file is reached. The function will not produce an error if a closing
/// delimiter is found.
fn peek_delims(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    delims: &[TokenType],
    context: &[TokenType],
    eat_lines: bool,
    error_on_eof: bool,
    error_message: &'static str,
) -> Option<(usize, TokenType)> {
    loop {
        match peek_token(tokens, eat_lines) {
            Some((i, token)) if delims.contains(token.token_type()) => {
                return Some((i, token.token_type().clone()));
            }
            Some((_, token)) if is_closing(token.token_type(), context) => {
                return None;
            }
            None => {
                if error_on_eof {
                    errors.push(ParseError::at_end(error_message));
                }

                return None;
            }
            Some((i, _)) => {
                errors.push(ParseError::single(error_message, i));
                next_token(tokens, true).expect("The token has already been peeked");
            }
        }
    }
}

/// Parse a condition, defined as:
///
/// '(' expr_or_help ')'
///
/// Note that `ifcond` is identically defined, and so this function is used.
fn parse_condition(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
) -> Node {
    let context = &add_context(context, TokenType::RightParen);

    let (start, _) = match peek_token(tokens, true) {
        Some((_, token)) if token.token_type() == &TokenType::LeftParen => {
            next_token(tokens, true).expect("The token has already been peeked")
        }
        x => {
            errors.push(ParseError::new(
                "Expected '('",
                x.map(|x| Span::single(x.0)),
            ));
            return Node::empty(NodeType::FormList);
        }
    };

    let cond = parse_expr(tokens, errors, 0, true, false, true, context);

    let delim_end = expect_delim(
        tokens,
        errors,
        TokenType::RightParen,
        context,
        "Expected ')'",
    );

    let end = delim_end.unwrap_or(cond.end().unwrap_or(start));

    Node::new(
        NodeType::Condition,
        Span::new(start, end),
        vec![cond],
        delim_end.is_none(),
    )
}

/// Parses a for condition, defined as
///
/// '(' SYMBOL IN expr_or_help ')'
fn parse_for_condition(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
) -> Node {
    let context = &add_context(context, TokenType::RightParen);

    let (start, _) = match peek_token(tokens, true) {
        Some((_, token)) if token.token_type() == &TokenType::LeftParen => {
            next_token(tokens, true).expect("The token has already been peeked")
        }
        x => {
            errors.push(ParseError::new(
                "Expected '('",
                x.map(|x| Span::single(x.0)),
            ));
            return Node::empty(NodeType::FormList);
        }
    };

    let lhs = parse_expr(tokens, errors, 0, true, false, true, context);

    let in_op = expect_delim(tokens, errors, TokenType::In, context, "Expected 'in'");

    let rhs = parse_expr(tokens, errors, 0, true, false, true, context);

    let delim_end = expect_delim(
        tokens,
        errors,
        TokenType::RightParen,
        context,
        "Expected ')",
    );

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

/// Skip all non-useful tokens, including newlines if `eat_lines` is `true`.
///
/// Skips whitespace, comments and error tokens.
fn skip_whitespace(tokens: &mut Tokens, eat_lines: bool) {
    while tokens
        .next_if(|(_, token)| {
            token.token_type() == &TokenType::WhiteSpace
                || token.token_type() == &TokenType::Comment
                || token.token_type() == &TokenType::Error
                || (eat_lines && token.token_type() == &TokenType::NewLine)
        })
        .is_some()
    {}
}

/// Skips all whitespace after an if condition. Skips all newlines (even if `eat_lines` is false) if
/// the next useful token is `else`.
fn skip_whitespace_after_if(tokens: &mut Tokens, eat_lines: bool) {
    if eat_lines {
        skip_whitespace(tokens, true);
        return;
    }

    let next_token = tokens.clone().find(|(_, token)| {
        !matches!(
            token.token_type(),
            &TokenType::WhiteSpace | &TokenType::NewLine | &TokenType::Comment | &TokenType::Error
        )
    });

    if next_token.is_some_and(|(_, token)| token.token_type() == &TokenType::Else) {
        skip_whitespace(tokens, true)
    } else {
        skip_whitespace(tokens, false)
    }
}

/// Parses the list of formal arguments / parameters to a function
///
/// '(' formlist ')'
///
/// formlist: SYMBOL
///     | SYMBOL EQ_ASSIGN expr_or_help
///     | formlist ',' SYMBOL
///     | formlist ',' SYMBOL EQ_ASSIGN expr_or_help
fn parse_formlist(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    eat_lines_init: bool,
    context: &[TokenType],
) -> Node {
    let (start, _) = match peek_token(tokens, eat_lines_init) {
        Some((_, token)) if token.token_type() == &TokenType::LeftParen => {
            next_token(tokens, true).expect("The token has already been peeked")
        }
        x => {
            errors.push(ParseError::new(
                "Expected '('",
                x.map(|x| Span::single(x.0)),
            ));
            return Node::empty(NodeType::FormList);
        }
    };

    parse_list(
        tokens,
        errors,
        NodeType::FormList,
        false,
        start,
        context,
        parse_formlist_item,
    )
}

/// Parses a list of arguments to a function, defined as the following expressions:
///
/// '(' sublist ')'
/// '[' sublist ']'
///
/// sublist: sub | sublist ',' sub
///
/// sub: expr_or_help
///     | SYMBOL EQ_ASSIGN
///     | SYMBOL EQ_ASSIGN expr_or_help
///     | STR_CONST EQ_ASSIGN
///     | STR_CONST EQ_ASSIGN expr_or_help
///     | NULL_CONST EQ_ASSIGN
///     | NULL_CONST EQ_ASSIGN expr_or_help
fn parse_sublist(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
    square: bool,
    start: usize,
) -> Node {
    parse_list(
        tokens,
        errors,
        NodeType::SubList,
        square,
        start,
        context,
        parse_sublist_item,
    )
}

/// Parses either a sublist or a formlist
fn parse_list(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    node_type: NodeType,
    square: bool,
    start: usize,
    context: &[TokenType],
    parse_item: impl Fn(&mut Tokens, &mut Vec<ParseError>, &[TokenType]) -> Option<Node>,
) -> Node {
    let context = &add_contexts(
        context,
        &[
            if square {
                TokenType::RightSquare
            } else {
                TokenType::RightParen
            },
            TokenType::Comma,
        ],
    );

    let (_, rhs) = get_delimiters(square);

    let mut args = Vec::new();
    let delims = &[rhs.clone(), TokenType::Comma];

    loop {
        if let Some(item) = parse_item(tokens, errors, context) {
            args.push(item);
        }

        let delim = peek_delims(
            tokens,
            errors,
            delims,
            context,
            true,
            true,
            if square {
                "Expected ']' or ','"
            } else {
                "Expected ')' or ','"
            },
        );

        match delim {
            Some((_, TokenType::Comma)) => {
                next_token(tokens, true).expect("The token has already been peeked");
            }
            Some((_, token_type)) if is_closing(&token_type, context) => break,
            Some(_) => {}
            None => break,
        };
    }

    let final_delim = expect_delim(
        tokens,
        errors,
        rhs,
        context,
        if square {
            "Expected ']'"
        } else {
            "Expected ')'"
        },
    );

    let end = final_delim.unwrap_or(args.last().and_then(|x| x.end()).unwrap_or(start));

    Node::new(
        node_type,
        Span::new(start, end),
        args,
        final_delim.is_none(),
    )
}

/// Parses an item of a sublist
fn parse_sublist_item(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
) -> Option<Node> {
    if peek_token(tokens, true).is_some_and(|(_, token)| {
        token.token_type() == &TokenType::Comma || is_closing(token.token_type(), context)
    }) {
        return None;
    }

    let lhs = parse_expr(tokens, errors, 0, true, false, true, context);

    let equals_index = match peek_token(tokens, true) {
        Some((index, token)) if token.token_type() == &TokenType::Equals => index,
        _ => return Some(Node::wraps(NodeType::SubListItem, lhs)),
    };

    next_token(tokens, true).expect("The token has already been peeked");

    let lhs = if !matches!(
        lhs.node_type(),
        &NodeType::Symbol { .. } | &NodeType::LiteralString { .. } | &NodeType::Null
    ) {
        // TODO: Error and continue
        errors.push(ParseError::new(
            "Expected a symbol, string, or NULL",
            Some(lhs.span().clone()),
        ));

        lhs.as_error()
    } else {
        lhs
    };

    let start = lhs.start().unwrap_or(equals_index);
    if peek_token(tokens, true).is_some_and(|(_, token)| is_closing(token.token_type(), context)) {
        return Some(Node::ok(
            NodeType::SubListItem,
            Span::new(start, equals_index),
            vec![lhs],
        ));
    }

    let rhs = parse_expr(tokens, errors, 0, true, false, true, context);
    let end = rhs.end().unwrap_or(equals_index);
    Some(Node::ok(
        NodeType::SubListItem,
        Span::new(start, end),
        vec![lhs, rhs],
    ))
}

/// Parses an item of a formlist
fn parse_formlist_item(
    tokens: &mut Tokens,
    errors: &mut Vec<ParseError>,
    context: &[TokenType],
) -> Option<Node> {
    if peek_token(tokens, true).is_some_and(|(_, token)| {
        token.token_type() == &TokenType::Comma || is_closing(token.token_type(), context)
    }) {
        return None;
    }

    let lhs = parse_expr(tokens, errors, 0, true, false, true, context);

    let lhs = if !matches!(lhs.node_type(), NodeType::Symbol { .. }) {
        errors.push(ParseError::new(
            "Expected a symbol, string, or NULL",
            Some(lhs.span().clone()),
        ));

        lhs.as_error()
    } else {
        lhs
    };

    let equals_index = match peek_token(tokens, true) {
        Some((index, token)) if token.token_type() == &TokenType::Equals => index,
        _ => return Some(Node::wraps(NodeType::SubListItem, lhs)),
    };

    next_token(tokens, true).expect("The token has already been peeked");

    let rhs = parse_expr(tokens, errors, 0, true, true, true, context);

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

/// Determines the order of operations for infix operators.
///
/// If an operation has a larger binding power on its right than on its left, it
/// is left associative. For example, if + has power (1, 2):
/// 1   +   2   +   3
///  (1) (2) (1) (2)
/// = (1 + 2) + 3
///
/// If an operation is not associative, use the same binding power to cause an error if
/// two are used in a row.
///
/// https://rdrr.io/r/base/Syntax.html
fn infix_binding_power(token: &TokenType) -> Option<(u8, u8)> {
    match *token {
        TokenType::Help => Some((1, 2)),
        TokenType::Equals => Some((10, 9)),
        TokenType::LeftAssign | TokenType::DoubleLeftAssign | TokenType::ColonEquals => {
            Some((12, 11))
        }
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

/// Determines the binding power of prefix operators
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

/// Determines whether a token closes a context
fn is_closing(token_type: &TokenType, context: &[TokenType]) -> bool {
    match token_type {
        TokenType::SemiColon => true,
        _ if context.contains(token_type) => true,
        _ => false,
    }
}

/// Immutably appends a token type to a context
fn add_context(context: &[TokenType], value: TokenType) -> Vec<TokenType> {
    let mut vec = context.to_vec();
    vec.push(value);
    vec
}

fn add_contexts(context: &[TokenType], values: &[TokenType]) -> Vec<TokenType> {
    let mut vec = context.to_vec();
    vec.extend_from_slice(values);
    vec
}
