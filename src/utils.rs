use std::str::FromStr;

use anyhow::Result;
use camino::Utf8PathBuf;
use url::Url;

use crate::{
    grammar::TokenType,
    nodes::{Node, NodeType},
};

pub fn parse_url(uri: lsp_types::Uri) -> Result<Utf8PathBuf> {
    let url = Url::parse(uri.as_str())?;

    let path = match url.to_file_path() {
        Ok(x) => x,
        Err(_) => {
            return Err(anyhow::anyhow!("Invalid file path: {:?}", url));
        }
    };

    Ok(camino::absolute_utf8(&path)?)
}

pub fn path_to_uri(path: &Utf8PathBuf) -> Result<lsp_types::Uri> {
    file_to_uri(path.as_str())
}

pub fn file_to_uri(path: &str) -> Result<lsp_types::Uri> {
    let url = match Url::from_file_path(path) {
        Ok(x) => x,
        Err(_) => {
            anyhow::bail!("Invalid file path: {:?}", path);
        }
    };

    Ok(lsp_types::Uri::from_str(url.as_str())?)
}

pub fn split_assignment(node: &Node) -> Option<(&Node, &Node)> {
    match node.node_type() {
        NodeType::Binary { op, lhs, rhs } => match op.token_type() {
            TokenType::Equals | TokenType::LeftAssign => Some((lhs, rhs)),
            TokenType::RightAssign => Some((rhs, lhs)),
            _ => None,
        },
        _ => None,
    }
}

pub struct Arg<'a> {
    pub lhs: Option<&'a Node>,
    pub rhs: Option<&'a Node>,
    pub node: &'a Node,
}

pub fn iter_args(node: &Node) -> impl Iterator<Item = Arg> {
    match node.node_type() {
        NodeType::FormList { items } => items.iter().map(|x| into_arg(x)),
        _ => panic!("Expected form list"),
    }
}

fn into_arg(node: &Node) -> Arg {
    match node.node_type() {
        NodeType::FormListItem { lhs, rhs } => Arg {
            lhs: Some(lhs),
            rhs: rhs.as_deref(),
            node,
        },
        NodeType::SubListItem { lhs, rhs } => Arg {
            lhs: lhs.as_deref(),
            rhs: rhs.as_deref(),
            node,
        },
        _ => panic!("Expected form list item"),
    }
}
