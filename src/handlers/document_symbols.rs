use itertools::Itertools;
use lsp_types::DocumentSymbol;

use crate::{
    file::SourceFile,
    grammar::TokenType,
    nodes::{Node, NodeType},
    server::Server,
};

use anyhow::Result;

pub fn document_symbols(
    server: &Server,
    params: lsp_types::DocumentSymbolParams,
) -> Result<Vec<DocumentSymbol>> {
    let file = server.get_file(params.text_document.uri)?;

    let root = file.get_parse_tree();

    let children: Vec<_> = root
        .children()
        .into_iter()
        .filter(|x| !x.is_error())
        .collect();

    let mut document_symbols = Vec::new();

    for child in children {
        if let Some((assignee, assigned)) = get_parts(child) {
            let symbol = document_symbol(file, child, assignee, assigned);
            document_symbols.push(symbol);
        }
    }

    Ok(document_symbols)
}

pub fn get_parts(node: &Node) -> Option<(&Node, &Node)> {
    let binary_op = match node.node_type() {
        NodeType::Binary { op: x, .. } => x,
        _ => return None,
    };

    let children = node.children();

    let (assignee, assigned) = match binary_op.token_type() {
        &TokenType::Equals | &TokenType::LeftAssign => (children.get(0)?, children.get(1)?),
        &TokenType::RightAssign => (children.get(1)?, children.get(0)?),
        _ => return None,
    };

    if assignee.is_error() || assigned.is_error() {
        return None;
    }

    match assignee.node_type() {
        NodeType::Symbol { value: _ } | NodeType::LiteralString { value: _ } => {
            Some((assignee, assigned))
        }
        _ => None,
    }
}

pub fn document_symbol(
    file: &SourceFile,
    node: &Node,
    assignee: &Node,
    assigned: &Node,
) -> lsp_types::DocumentSymbol {
    let tokens = file.get_tokens();

    let name = match assignee.node_type() {
        NodeType::Symbol { value } => value.clone(),
        NodeType::LiteralString { value } => value.clone(),
        _ => panic!("Assignee must be a symbol or string"),
    };

    let detail = get_detail(file, &name, assigned);

    let kind = get_type(assigned);

    let range = node.text_span(tokens).unwrap();

    let selection_range = assigned.text_span(tokens).unwrap();

    #[allow(deprecated)]
    lsp_types::DocumentSymbol {
        name,
        detail,
        kind,
        tags: None,
        deprecated: None,
        range: range.into(),
        selection_range: selection_range.into(),
        children: None,
    }
}

fn get_detail(file: &SourceFile, name: &str, node: &Node) -> Option<String> {
    if let NodeType::Function { args, body: _ } = node.node_type() {
        let params = args
            .children()
            .iter()
            .filter_map(|x| format_param(file, x))
            .join(", ");

        Some(format!("{}({})", name, params))
    } else {
        None
    }
}

fn format_param(file: &SourceFile, param: &Node) -> Option<String> {
    if let NodeType::FormListItem { lhs, rhs } = param.node_type() {
        let lhs_text = file.get_node_text(lhs)?.to_string();

        if let Some(rhs) = rhs {
            if !rhs.is_error() {
                let rhs_text = file.get_node_text(rhs)?.to_string();
                return Some(format!("{} = {}", lhs, rhs_text));
            }
        }

        Some(lhs_text)
    } else {
        None
    }
}

fn get_type(node: &Node) -> lsp_types::SymbolKind {
    match *node.node_type() {
        NodeType::Function { .. } => lsp_types::SymbolKind::FUNCTION,
        NodeType::Symbol { .. } => lsp_types::SymbolKind::VARIABLE,
        NodeType::LiteralString { .. } => lsp_types::SymbolKind::STRING,
        NodeType::LiteralNumber => lsp_types::SymbolKind::NUMBER,
        NodeType::LiteralBool { .. } => lsp_types::SymbolKind::BOOLEAN,
        _ => lsp_types::SymbolKind::VARIABLE,
    }
}
