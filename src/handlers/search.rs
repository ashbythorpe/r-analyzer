use crate::{
    cursor::Cursor,
    grammar::TokenType,
    nodes::{Node, NodeType},
};

pub enum SearchPattern {
    Exact(String),
    StartsWith(String),
}

pub fn search<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, pattern: &SearchPattern) {
    match node.node_type() {
        NodeType::PrefixCall { rhs } => search(nodes, rhs, pattern),
        NodeType::Parentheses { contents } => search(nodes, contents, pattern),
        NodeType::Braces { exprs } => exprs.iter().rev().for_each(|e| search(nodes, e, pattern)),
        NodeType::If {
            condition,
            consequent_expr,
            alternative_expr,
        } => {
            if let Some(alternative) = alternative_expr {
                search(nodes, alternative, pattern);
            }
            search(nodes, consequent_expr, pattern);
            search_condition(nodes, condition, pattern);
        }
        NodeType::For { condition, expr } => {
            search(nodes, expr, pattern);
            search_condition(nodes, condition, pattern);
        }
        NodeType::While { condition, expr } => {
            search(nodes, expr, pattern);
            search_condition(nodes, condition, pattern);
        }
        NodeType::Repeat { expr } => search(nodes, expr, pattern),
        NodeType::Call { function: x, args } => {
            search_sublist(nodes, args, pattern);

            search(nodes, x, pattern);
        }
        NodeType::Subset { lhs, args } => {
            search_sublist(nodes, args, pattern);

            search(nodes, lhs, pattern);
        }
        NodeType::Index { lhs, args } => {
            search_sublist(nodes, args, pattern);

            search(nodes, lhs, pattern);
        }
        NodeType::Extract { lhs, rhs: _ } => {
            search(nodes, lhs, pattern);
        }
        NodeType::Binary { lhs, rhs, op } => match op.token_type() {
            TokenType::LeftAssign | TokenType::Equals => {
                if node_matches(lhs, pattern) {
                    nodes.push(node);
                }
            }
            TokenType::RightAssign => {
                if node_matches(rhs, pattern) {
                    nodes.push(node);
                }
            }
            _ => {
                search(nodes, rhs, pattern);
                search(nodes, lhs, pattern);
            }
        },
        _ => (),
    }
}

pub fn node_matches(node: &Node, name: &SearchPattern) -> bool {
    let value = match node.node_type() {
        NodeType::Symbol { value } => value,
        NodeType::LiteralString { value } => value,
        _ => return false,
    };

    match name {
        SearchPattern::Exact(v) => v == value,
        SearchPattern::StartsWith(v) => value.starts_with(v),
    }
}

pub fn search_condition<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, name: &SearchPattern) {
    match node.node_type() {
        NodeType::Condition { expr } => search(nodes, expr, name),
        NodeType::ForCondition { lhs: _, rhs } => search(nodes, rhs, name),
        _ => panic!("Expected condition"),
    }
}

fn search_sublist<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, name: &SearchPattern) {
    match node.node_type() {
        NodeType::SubList { items } => {
            for item in items.iter().rev() {
                match item.node_type() {
                    NodeType::SubListItem { lhs: _, rhs } => {
                        if let Some(rhs) = rhs {
                            search(nodes, rhs, name);
                        }
                    }
                    _ => panic!("Expected sublist item"),
                }
            }
        }
        _ => panic!("Expected sublist"),
    }
}

pub fn step_out<'a>(
    nodes: &mut Vec<&'a Node>,
    mut cursor: Cursor<'a>,
    pattern: &SearchPattern,
) -> Cursor<'a> {
    let node = cursor.current();

    cursor.go_to_parent().expect("Symbol must have a parent");

    let parent = cursor.current();

    match parent.node_type() {
        NodeType::Condition { expr: _ } => {
            cursor
                .go_to_parent()
                .expect("Condition symbol must have a parent");
        }
        NodeType::If {
            condition,
            consequent_expr: _,
            alternative_expr: _,
        } => search_condition(nodes, condition, pattern),
        NodeType::For {
            condition: _,
            expr: _,
        } => search_condition(nodes, node, pattern),
        NodeType::While {
            condition: _,
            expr: _,
        } => search_condition(nodes, node, pattern),
        NodeType::Function { args, body: _ } => {
            search_args(nodes, args, pattern);
        }
        NodeType::Call { function, args: _ } => {
            if node != function.as_ref() {
                search_args(nodes, function, pattern);
            }
        }
        NodeType::Subset { lhs, args: _ } => {
            if node != lhs.as_ref() {
                search_args(nodes, lhs, pattern);
            }
        }
        NodeType::Index { lhs, args: _ } => {
            if node != lhs.as_ref() {
                search_args(nodes, lhs, pattern);
            }
        }
        NodeType::ForCondition { lhs, rhs: _ } => match lhs.node_type() {
            NodeType::ForCondition { lhs: _, rhs } => {
                search(nodes, rhs, pattern);

                if node_matches(lhs, pattern) {
                    nodes.push(parent);
                }
            }
            _ => panic!("Expected for condition"),
        },
        NodeType::ErrorBoundary { node: _ } => {
            cursor
                .go_to_parent()
                .expect("Error boundary symbol must have a parent");
        }
        _ => {}
    }

    cursor
}

fn search_args<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, name: &SearchPattern) {
    match node.node_type() {
        NodeType::FormList { items } => {
            for item in items.iter().rev() {
                match item.node_type() {
                    NodeType::FormListItem { lhs, rhs: _ } => {
                        if node_matches(lhs, name) {
                            nodes.push(item);
                        }
                    }
                    _ => panic!("Expected form list item"),
                }
            }
        }
        _ => panic!("Expected form list"),
    }
}
