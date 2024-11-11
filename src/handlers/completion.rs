use anyhow::Result;
use fst::{map::OpBuilder, raw::IndexedValue, IntoStreamer, Streamer};
use itertools::Itertools;
use log::info;
use lsp_types::CompletionList;

use crate::{
    cursor::node_at_position,
    grammar::{FilePosition, FileSpan, TokenType},
    package_index::Symbol,
    server::{FileContext, Server},
    workspace::WorkSpace,
};

use super::search::{search, step_out, SearchPattern};

pub fn propose_completions(
    server: &Server,
    params: lsp_types::CompletionParams,
) -> Result<Option<lsp_types::CompletionList>> {
    let context = server.file_context(&params.text_document_position.text_document.uri)?;

    let file_position: FilePosition = params.text_document_position.position.into();
    let position = FilePosition::new(
        file_position.line,
        if file_position.column > 0 {
            file_position.column - 1
        } else {
            file_position.column
        },
    );

    let token = context.source_file().token_at(position);
    let mut cursor = node_at_position(context.source_file(), position);

    info!("Token: {:?}", token.token_type());
    info!("Node: {:?}", cursor.current());

    let value = match token.token_type() {
        TokenType::Symbol => {
            let span = FileSpan::new(token.span().start, position);

            context.source_file().get_file_span(&span)
        }
        _ => return Ok(None),
    };

    let pattern = SearchPattern::StartsWith(value.to_string());

    let mut nodes = Vec::new();

    while !cursor.is_top_level() {
        while cursor.go_to_previous_sibling().is_ok() {
            let node = cursor.current();

            info!("Searching for definitions");
            search(&mut nodes, node, &pattern);
        }

        cursor = step_out(&mut nodes, cursor, &pattern);
    }

    let mut items: Vec<_> = nodes
        .into_iter()
        .map(|x| lsp_types::CompletionItem {
            label: x.to_string(),
            kind: Some(lsp_types::CompletionItemKind::VARIABLE),
            ..Default::default()
        })
        .collect();

    items.extend(query(server, &context, &value.to_string()));

    Ok(Some(CompletionList {
        is_incomplete: false,
        items,
    }))
}

fn query(server: &Server, context: &FileContext, name: &str) -> Vec<lsp_types::CompletionItem> {
    let mut stream = OpBuilder::new();

    let package_index = server.package_index();

    let exported: Vec<_> = package_index.get_package("base").into_iter().collect();

    for package in &exported {
        stream.push(package.query_subsequence(name));
    }

    let files = match context.workspace() {
        WorkSpace::SingleFile(x) => {
            stream.push(x.index().query_subsequence(name));
            None
        }
        WorkSpace::MultiFile(x) => {
            let index = x.symbol_index();
            let files: Vec<_> = index.symbols().iter().collect();

            for (_, file) in &files {
                stream.push(file.query_subsequence(name));
            }

            Some(files)
        }
    };

    let mut union = stream.union().into_stream();

    let mut results = Vec::new();

    while let Some((_, indexed_values)) = union.next() {
        for IndexedValue { index, value } in indexed_values {
            if let Some(package) = exported.get(*index) {
                let symbol = package.get_exported_symbol(*value);

                let item = match symbol {
                    Symbol::Function { name, signature } => {
                        let signature = signature
                            .iter()
                            .map(|x| match x.default() {
                                Some(default) => format!("{} = {}", x.name(), default),
                                None => x.name().to_string(),
                            })
                            .join(", ");

                        let label = if package.name() == "base" {
                            name.to_string()
                        } else {
                            format!("{}::{}", package.name(), name)
                        };

                        lsp_types::CompletionItem {
                            label,
                            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                            detail: Some(signature),
                            ..Default::default()
                        }
                    }
                    Symbol::Object { name } => {
                        let label = if package.name() == "base" {
                            name.to_string()
                        } else {
                            format!("{}::{}", package.name(), name)
                        };

                        lsp_types::CompletionItem {
                            label,
                            kind: Some(lsp_types::CompletionItemKind::VALUE),
                            ..Default::default()
                        }
                    }
                };

                results.push(item);
            } else {
                let symbol = match context.workspace() {
                    WorkSpace::SingleFile(x) => x
                        .index()
                        .get_symbol(*value as usize)
                        .expect("Symbol not found"),
                    WorkSpace::MultiFile(_) => {
                        let files = files.as_ref().unwrap();

                        files
                            .get(*index)
                            .expect("File not found")
                            .1
                            .get_symbol(*value as usize)
                            .expect("Symbol not found")
                    }
                };

                let item = match symbol.symbol() {
                    Symbol::Function { name, signature } => {
                        let signature = signature
                            .iter()
                            .map(|x| match x.default() {
                                Some(default) => format!("{} = {}", x.name(), default),
                                None => x.name().to_string(),
                            })
                            .join(", ");

                        lsp_types::CompletionItem {
                            label: name.to_string(),
                            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                            detail: Some(signature),
                            ..Default::default()
                        }
                    }
                    Symbol::Object { name } => lsp_types::CompletionItem {
                        label: name.to_string(),
                        kind: Some(lsp_types::CompletionItemKind::VALUE),
                        ..Default::default()
                    },
                };

                results.push(item);
            }
        }
    }

    results
}
