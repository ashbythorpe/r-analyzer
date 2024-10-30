use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::Result;
use url::Url;

use crate::{
    grammar::TokenType,
    nodes::{Node, NodeType},
};

pub fn parse_url(uri: lsp_types::Uri) -> Result<PathBuf> {
    let url = Url::parse(uri.as_str())?;

    let path = match url.to_file_path() {
        Ok(x) => x,
        Err(_) => {
            return Err(anyhow::anyhow!("Invalid file path: {:?}", url));
        }
    };

    Ok(path.canonicalize()?)
}

pub fn path_to_uri<P>(path: P) -> Result<lsp_types::Uri>
where
    P: AsRef<Path>,
{
    let url = match Url::from_file_path(path) {
        Ok(x) => x,
        Err(_) => {
            anyhow::bail!("Invalid file path");
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
