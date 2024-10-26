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
        if let Some(symbol) = document_symbol(file, child) {
            document_symbols.push(symbol);
        }
    }

    Ok(document_symbols)
}

pub fn document_symbol(file: &SourceFile, node: &Node) -> Option<lsp_types::DocumentSymbol> {
    let tokens = file.get_tokens();

    let (assignee, assigned) = match node.node_type() {
        NodeType::Binary { op: _, lhs, rhs } if lhs.is_error() || rhs.is_error() => return None,
        NodeType::Binary { op, lhs, rhs }
            if matches!(*op.token_type(), TokenType::Equals | TokenType::LeftAssign) =>
        {
            (lhs, rhs)
        }
        NodeType::Binary { op, lhs, rhs } if matches!(*op.token_type(), TokenType::RightAssign) => {
            (rhs, lhs)
        }
        _ => return None,
    };

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
    Some(lsp_types::DocumentSymbol {
        name,
        detail,
        kind,
        tags: None,
        deprecated: None,
        range: range.into(),
        selection_range: selection_range.into(),
        children: None,
    })
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
        panic!("Every child of a formlist must be a formlistitem");
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
