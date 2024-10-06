use anyhow::Result;

use crate::{
    cursor::{node_covering, NoParentError},
    file::SourceFile,
    grammar::{FilePosition, FileSpan, TokenType},
    server::Server,
};

pub fn selection_range(
    server: &Server,
    params: lsp_types::SelectionRangeParams,
) -> Result<Vec<lsp_types::SelectionRange>> {
    let file = server.get_file(params.text_document.uri)?;

    Ok(params
        .positions
        .iter()
        .map(|position| get_ranges(file, (*position).into()))
        .flat_map(create_selection_range)
        .collect())
}

fn create_selection_range(ranges: Vec<FileSpan>) -> Option<lsp_types::SelectionRange> {
    let mut result = lsp_types::SelectionRange {
        range: ranges.last()?.to_owned().into(),
        parent: None,
    };

    for range in ranges.iter().rev().skip(1) {
        result = lsp_types::SelectionRange {
            range: range.to_owned().into(),
            parent: Some(Box::new(result)),
        }
    }

    Some(result)
}

fn get_ranges(file: &SourceFile, position: FilePosition) -> Vec<FileSpan> {
    let initial_range = FileSpan::between(position, position);
    let parse_tree = file.get_parse_tree();
    let tokens = file.get_tokens();

    if parse_tree.is_empty() {
        return vec![];
    }

    let token_index = file.token_index_at(position);
    let token = &file.get_tokens()[token_index];
    let token_range = token.span().to_owned();

    let mut ranges = if initial_range == token_range {
        vec![initial_range]
    } else {
        vec![initial_range, token_range]
    };

    let mut range = token_range;

    if *token.token_type() == TokenType::WhiteSpace {
        let whitespace_range = expand_whitespace(file, token_index);

        if whitespace_range != range {
            ranges.push(whitespace_range);

            range = whitespace_range;
        }
    }

    let mut cursor = node_covering(file, range)
        .unwrap_or_else(|_| panic!("Node covering {:?} not found", range));

    loop {
        if cursor.text_span(file.get_tokens()).unwrap() == range {
            match cursor.go_to_parent() {
                Ok(_) => {}
                Err(NoParentError) => {
                    let range = parse_tree.text_span(tokens).unwrap();
                    ranges.push(range);
                    break;
                }
            }
        }

        let next = cursor.text_span(file.get_tokens()).unwrap();

        if next != range {
            ranges.push(next);
        }

        range = next;
    }

    ranges
}

fn expand_whitespace(file: &SourceFile, token_index: usize) -> FileSpan {
    let tokens = file.get_tokens();
    let mut start_index = token_index;

    while tokens[start_index].token_type() == &TokenType::WhiteSpace {
        start_index -= 1;
    }

    let mut start_index = token_index;

    while tokens[start_index].token_type() == &TokenType::WhiteSpace {
        start_index += 1;
    }

    FileSpan::between_spans(tokens[start_index].span(), tokens[token_index].span())
}
