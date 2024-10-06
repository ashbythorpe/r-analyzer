use ropey::Rope;

use crate::{
    grammar::Token,
    nodes::{Node, NodeType},
};

struct TextEdit {
    start: usize,
    end: usize,
    text: String,
}

pub fn format(input: &Rope, tokens: Vec<Token>, syntax_tree: Vec<Node>) -> Vec<TextEdit> {
    unimplemented!();

    let mut edits = Vec::new();

    let mut indent = 0;

    edits
}

pub fn format_node(
    input: &Rope,
    tokens: &Vec<Token>,
    node: &Node,
    indent: usize,
    edits: &mut Vec<TextEdit>,
) {
    if !should_format(node, tokens) {
        return;
    }

    match node.node_type() {
        NodeType::Condition => {}
        NodeType::Expr => todo!(),
        NodeType::Symbol { value } => todo!(),
        NodeType::LiteralNumber => todo!(),
        NodeType::LiteralString { value } => todo!(),
        NodeType::Null => todo!(),
        NodeType::Placeholder => todo!(),
        NodeType::PrefixCall => todo!(),
        NodeType::Parentheses => todo!(),
        NodeType::Braces => todo!(),
        NodeType::If => todo!(),
        NodeType::For => todo!(),
        NodeType::While => todo!(),
        NodeType::Repeat => todo!(),
        NodeType::Function => todo!(),
        NodeType::Next => todo!(),
        NodeType::Break => todo!(),
        NodeType::Call => todo!(),
        NodeType::Subset => todo!(),
        NodeType::Index => todo!(),
        NodeType::NameSpace { internal } => todo!(),
        NodeType::Extract => todo!(),
        NodeType::Binary { op } => todo!(),
        NodeType::ForCondition => todo!(),
        NodeType::FormList => todo!(),
        NodeType::FormListItem => todo!(),
        NodeType::SubList => todo!(),
        NodeType::SubListItem => todo!(),
        NodeType::WhiteSpace => todo!(),
        NodeType::File => todo!(),
    }

    unimplemented!();
}

fn format_condition(node: &Node, tokens: Vec<Token>, indent: usize, edits: &mut Vec<TextEdit>) {
    unimplemented!();
}

fn should_format(node: &Node, tokens: &Vec<Token>) -> bool {
    if node.is_error()
        || node
            .relevent_tokens(tokens)
            .iter()
            .any(|token| token.is_error())
    {
        return false;
    }

    match node.node_type() {
        NodeType::Braces => true,
        _ => node.children().iter().all(|child| !child.is_error()),
    }
}

