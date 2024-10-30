use anyhow::Result;
use std::{
    collections::{hash_map::Entry, HashMap},
    path::PathBuf,
};

use crate::{
    file::SourceFile,
    grammar::TokenType,
    nodes::{Node, NodeType},
    package_index::{Arg, Symbol},
    server::Server,
    utils::path_to_uri,
};

pub struct SymbolIndex {
    symbols: HashMap<PathBuf, FileIndex>,
    index: HashMap<String, (PathBuf, usize)>,
}

pub struct FileIndex {
    symbols: Vec<FileSymbol>,
    index: fst::Map<Vec<u8>>,
}

pub struct FileSymbol {
    symbol: Symbol,
    file: PathBuf,
    node_index: usize,
}

impl FileSymbol {
    pub fn new(symbol: Symbol, file: PathBuf, node_index: usize) -> Self {
        Self {
            symbol,
            file,
            node_index,
        }
    }

    pub fn symbol(&self) -> &Symbol {
        &self.symbol
    }

    pub fn file(&self) -> &PathBuf {
        &self.file
    }

    pub fn node_index(&self) -> usize {
        self.node_index
    }

    pub fn get_node<'a>(&self, server: &'a Server) -> Result<&'a Node> {
        let tree = server.get_file(path_to_uri(self.file())?)?.get_parse_tree();

        Ok(tree.children()[self.node_index])
    }

    pub fn get_assignee<'a>(&self, server: &'a Server) -> Result<&'a Node> {
        let node = self.get_node(server)?;

        match node.node_type() {
            NodeType::Binary { op, lhs, rhs } => match op.token_type() {
                TokenType::Equals | TokenType::LeftAssign => Ok(lhs),
                TokenType::RightAssign => Ok(rhs),
                _ => panic!("Expected assignment operator"),
            },
            _ => panic!("Expected assignment operator"),
        }
    }
}

impl SymbolIndex {
    pub fn new(
        symbols: HashMap<PathBuf, FileIndex>,
        index: HashMap<String, (PathBuf, usize)>,
    ) -> Self {
        Self { symbols, index }
    }

    pub fn create(files: &HashMap<PathBuf, SourceFile>) -> Result<Self> {
        let mut symbols = HashMap::new();
        let mut index = HashMap::new();

        for (path, file) in files {
            let file_index = FileIndex::create(path, file)?;

            for symbol in file_index.symbols() {
                index.insert(
                    symbol.symbol.name().to_owned(),
                    (path.to_owned(), symbol.node_index),
                );
            }

            symbols.insert(path.clone(), file_index);
        }

        Ok(Self::new(symbols, index))
    }

    pub fn add_file(&mut self, path: PathBuf, file: &SourceFile) -> Result<()> {
        let file_index = FileIndex::create(&path, file)?;

        for symbol in file_index.symbols() {
            self.index.insert(
                symbol.symbol.name().to_owned(),
                (path.clone(), symbol.node_index),
            );
        }

        self.symbols.insert(path, file_index);

        Ok(())
    }

    pub fn remove_file(&mut self, path: &PathBuf) -> Result<()> {
        let file_index = match self.symbols.get(path) {
            Some(x) => x,
            None => return Ok(()),
        };

        for symbol in file_index.symbols() {
            let entry = self.index.entry(symbol.symbol.name().to_string());

            if let Entry::Occupied(x) = entry {
                if &x.get().0 == path {
                    x.remove();
                }
            }
        }

        Ok(())
    }

    pub fn update_file(&mut self, path: &PathBuf, new: &SourceFile) -> Result<()> {
        self.remove_file(path)?;
        self.add_file(path.to_owned(), new)
    }

    pub fn find_symbol(&self, name: &str) -> Option<&FileSymbol> {
        let (path, index) = self.index.get(name)?;

        self.symbols.get(path)?.symbols().get(*index)
    }
}

impl FileIndex {
    pub fn new(symbols: Vec<FileSymbol>, index: fst::Map<Vec<u8>>) -> Self {
        Self { symbols, index }
    }

    pub fn create(path: &PathBuf, file: &SourceFile) -> Result<Self> {
        let root = file.get_parse_tree();

        let symbols: Vec<_> = root
            .children()
            .into_iter()
            .filter(|x| !x.is_error())
            .enumerate()
            .filter_map(|(i, x)| document_symbol(path, file, i, x))
            .collect();

        let index = create_symbol_map(&symbols)?;

        Ok(Self::new(symbols, index))
    }

    pub fn symbols(&self) -> &[FileSymbol] {
        &self.symbols
    }

    pub fn index(&self) -> &fst::Map<Vec<u8>> {
        &self.index
    }
}

pub fn create_symbol_map(symbols: &[FileSymbol]) -> Result<fst::Map<Vec<u8>>> {
    Ok(fst::Map::from_iter(
        symbols
            .iter()
            .enumerate()
            .map(|(i, x)| (x.symbol.name(), i as u64)),
    )?)
}

pub fn document_symbol(
    path: &PathBuf,
    file: &SourceFile,
    index: usize,
    node: &Node,
) -> Option<FileSymbol> {
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

    if let NodeType::Function { args, body: _ } = assigned.node_type() {
        if let NodeType::FormList { items } = args.node_type() {
            let params = items.iter().filter_map(|x| format_param(file, x)).collect();

            return Some(FileSymbol {
                symbol: Symbol::Function {
                    name,
                    signature: params,
                },
                file: path.clone(),
                node_index: index,
            });
        }
    }

    Some(FileSymbol {
        symbol: Symbol::Object { name },
        file: path.clone(),
        node_index: index,
    })
}

fn format_param(file: &SourceFile, param: &Node) -> Option<Arg> {
    if let NodeType::FormListItem { lhs, rhs } = param.node_type() {
        let lhs_text = file.get_node_text(lhs)?.to_string();

        if let Some(rhs) = rhs {
            if !rhs.is_error() {
                let rhs_text = file
                    .get_node_text(rhs)
                    .expect("rhs cannot be empty")
                    .to_string();
                return Some(Arg::new(lhs_text, Some(rhs_text)));
            }
        }

        return Some(Arg::new(lhs_text, None));
    }

    None
}
