use std::path::PathBuf;

use anyhow::Result;
use lsp_types::{Location, Uri};
use memchr::memmem::Finder;

use crate::{
    cursor::{go_to_node, node_at_position, Cursor},
    file::SourceFile,
    grammar::FilePosition,
    nodes::{Node, NodeType},
    server::{FileContext, Server},
    utils::{iter_args, path_to_uri, split_assignment, Arg},
};

use super::definition::{get_definition, Definition};

pub fn find_references(
    server: &Server,
    params: lsp_types::ReferenceParams,
) -> Result<Vec<lsp_types::Location>> {
    let include_declaration = params.context.include_declaration;

    let context = server.file_context(&params.text_document_position.text_document.uri)?;

    let position = params.text_document_position.position.into();

    let definition = get_definition(server, &context, position)?;

    let exclude = if include_declaration {
        None
    } else {
        match definition {
            Some(Definition::FileSymbol {
                uri: _,
                file: _,
                node,
            }) => Some(node),
            _ => None,
        }
    };

    Ok(match definition {
        Some(Definition::FileSymbol { uri, file: _, node }) => {
            let context = server.file_context(&uri)?;
            node_references(&context, node, position, exclude)?
        }
        Some(Definition::PackageSymbol { package, name }) => {
            namespace_references(&context, package.clone(), name.clone(), exclude)?
        }
        Some(Definition::Param {
            function: _,
            arg: _,
        }) => {
            todo!()
        }
        None => Vec::new(),
    })
}

fn namespace_references(
    context: &FileContext,
    package: String,
    name: String,
    exclude: Option<&Node>,
) -> Result<Vec<Location>> {
    let pattern = Pattern::NameSpace { package, name };

    let mut references = Vec::new();

    let finder = pattern.finder();

    let workspace = context.workspace();

    for (path, file) in workspace.files() {
        search_file(
            path,
            file,
            &mut references,
            &finder,
            &pattern,
            context.uri(),
            exclude,
        )?;
    }

    Ok(references)
}

fn node_references<'a>(
    context: &'a FileContext,
    node: &'a Node,
    position: FilePosition,
    exclude: Option<&Node>,
) -> Result<Vec<Location>> {
    let original_node = node_at_position(context.source_file(), position).current();

    let name = match original_node.node_type() {
        NodeType::Placeholder => return Ok(Vec::new()),
        NodeType::Symbol { value } => value,
        NodeType::NameSpace { .. } => {
            panic!("The definition of a namespace can not be a file symbol")
        }
        NodeType::LiteralString { value } => value,
        _ => panic!("Invalid definition type"),
    };

    let pattern = if name == "..." {
        Pattern::Dots
    } else {
        Pattern::Name(name.to_string())
    };

    let mut cursor = go_to_node(context.source_file(), node);

    let mut references = Vec::new();

    if cursor.is_top_level() {
        let finder = pattern.finder();

        for (path, file) in context.workspace().files() {
            search_file(
                path,
                file,
                &mut references,
                &finder,
                &pattern,
                context.uri(),
                exclude,
            )?;
        }
    } else {
        match node.node_type() {
            NodeType::Binary { .. } => {
                let (_, rhs) = split_assignment(node).expect("Definition must be an assignment");

                search(
                    &mut references,
                    context.uri(),
                    context.source_file(),
                    cursor.to_child(rhs),
                    &pattern,
                    false,
                    None,
                );

                let mut cursor_right = cursor.clone();
                while cursor_right.go_to_next_sibling().is_ok() {
                    search(
                        &mut references,
                        context.uri(),
                        context.source_file(),
                        cursor_right.clone(),
                        &pattern,
                        false,
                        None,
                    );
                }

                let mut cursor_left = cursor.clone();
                while cursor_left.go_to_previous_sibling().is_ok() {
                    search(
                        &mut references,
                        context.uri(),
                        context.source_file(),
                        cursor_left.clone(),
                        &pattern,
                        false,
                        None,
                    );
                }

                // TODO: Search right for functions
            }
            NodeType::FormListItem { .. } => {
                cursor
                    .go_to_parent()
                    .expect("Form list item must have a parent");

                let mut function_cursor = cursor.clone();

                function_cursor
                    .go_to_parent()
                    .expect("Form list must have a parent");

                for Arg { rhs, node, lhs: _ } in iter_args(function_cursor.current()) {
                    if let Some(rhs) = rhs {
                        let mut arg_cursor = cursor.clone();

                        arg_cursor.go_to_child(node);
                        arg_cursor.go_to_child(rhs);

                        search(
                            &mut references,
                            context.uri(),
                            context.source_file(),
                            arg_cursor,
                            &pattern,
                            false,
                            None,
                        );
                    }
                }

                match function_cursor.current().node_type() {
                    NodeType::Function { args: _, body } => {
                        search(
                            &mut references,
                            context.uri(),
                            context.source_file(),
                            function_cursor.to_child(body),
                            &pattern,
                            false,
                            None,
                        );
                    }
                    _ => panic!("This node must be a function"),
                }
            }
            _ => panic!("Invalid definition node"),
        }
    }

    Ok(references)
}

fn search_file(
    path: &PathBuf,
    file: &SourceFile,
    references: &mut Vec<Location>,
    finder: &PatternFinder,
    pattern: &Pattern,
    original_uri: &Uri,
    exclude: Option<&Node>,
) -> Result<()> {
    let contents = file.get_content();

    let bytes: Vec<_> = contents.bytes().collect();

    let uri = path_to_uri(path)?;

    if &uri != original_uri && !finder.search(&bytes) {
        return Ok(());
    }

    let root = file.get_parse_tree();

    let cursor = Cursor::new(root);

    let exclude = if &uri == original_uri { exclude } else { None };

    search(references, &uri, file, cursor, pattern, false, exclude);

    Ok(())
}

fn search<'a>(
    references: &mut Vec<Location>,
    uri: &Uri,
    file: &'a SourceFile,
    cursor: Cursor<'a>,
    pattern: &Pattern,
    is_symbol: bool,
    exclude: Option<&'a Node>,
) {
    // TODO: Very poorly implemented
    // We need to:
    // 1. Check for variable shadowing
    // 2. Make decisions based on whether this is a function or not
    let node = cursor.current();

    match node.node_type() {
        NodeType::Condition { expr } => search(
            references,
            uri,
            file,
            cursor.to_child(expr),
            pattern,
            false,
            exclude,
        ),
        NodeType::Symbol { value: _ } => {
            if matches_pattern(node, pattern) && !exclude.is_some_and(|x| x == node) {
                references.push(Location {
                    uri: uri.clone(),
                    range: node
                        .text_span(file.get_tokens())
                        .expect("Node must have a span")
                        .into(),
                });
            }
        }
        NodeType::LiteralNumber => {}
        NodeType::LiteralString { value: _ } => {
            if is_symbol && matches_pattern(node, pattern) && !exclude.is_some_and(|x| x == node) {
                references.push(Location {
                    uri: uri.clone(),
                    range: node
                        .text_span(file.get_tokens())
                        .expect("Node must have a span")
                        .into(),
                });
            }
        }
        NodeType::LiteralBool { value: _ } => {}
        NodeType::Null => {}
        NodeType::Placeholder => {}
        NodeType::PrefixCall { rhs } => search(
            references,
            uri,
            file,
            cursor.to_child(rhs),
            pattern,
            false,
            exclude,
        ),
        NodeType::Parentheses { contents } => search(
            references,
            uri,
            file,
            cursor.to_child(contents),
            pattern,
            false,
            exclude,
        ),
        NodeType::Braces { exprs } => {
            for expr in exprs {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(expr),
                    pattern,
                    false,
                    exclude,
                )
            }
        }
        NodeType::If {
            condition,
            consequent_expr,
            alternative_expr,
        } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(condition),
                pattern,
                false,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(consequent_expr),
                pattern,
                false,
                exclude,
            );
            if let Some(alternative) = alternative_expr {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(alternative),
                    pattern,
                    false,
                    exclude,
                );
            }
        }
        NodeType::For { condition, expr } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(condition),
                pattern,
                false,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(expr),
                pattern,
                false,
                exclude,
            );
        }
        NodeType::While { condition, expr } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(condition),
                pattern,
                false,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(expr),
                pattern,
                false,
                exclude,
            );
        }
        NodeType::Repeat { expr } => search(
            references,
            uri,
            file,
            cursor.to_child(expr),
            pattern,
            false,
            exclude,
        ),
        NodeType::Function { args, body } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(args),
                pattern,
                false,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(body),
                pattern,
                false,
                exclude,
            );
        }
        NodeType::Next => {}
        NodeType::Break => {}
        NodeType::Call { function, args } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(function),
                pattern,
                true,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(args),
                pattern,
                false,
                exclude,
            );
        }
        NodeType::Subset { lhs, args } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(lhs),
                pattern,
                false,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(args),
                pattern,
                false,
                exclude,
            );
        }
        NodeType::Index { lhs, args } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(lhs),
                pattern,
                false,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(args),
                pattern,
                false,
                exclude,
            );
        }
        NodeType::NameSpace {
            internal: _,
            lhs: _,
            rhs: _,
        } => {
            if matches_pattern(node, pattern) && !exclude.is_some_and(|x| x == node) {
                references.push(Location {
                    uri: uri.clone(),
                    range: node
                        .text_span(file.get_tokens())
                        .expect("Node must have a span")
                        .into(),
                });
            }
        }
        NodeType::Extract { lhs, rhs: _ } => search(
            references,
            uri,
            file,
            cursor.to_child(lhs),
            pattern,
            false,
            exclude,
        ),
        NodeType::Binary { op: _, lhs, rhs } => {
            if let Some((lhs, rhs)) = split_assignment(node) {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(lhs),
                    pattern,
                    true,
                    exclude,
                );
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(rhs),
                    pattern,
                    false,
                    exclude,
                );
            } else {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(lhs),
                    pattern,
                    false,
                    exclude,
                );
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(rhs),
                    pattern,
                    false,
                    exclude,
                );
            }
        }
        NodeType::ForCondition { lhs, rhs } => {
            search(
                references,
                uri,
                file,
                cursor.to_child(lhs),
                pattern,
                true,
                exclude,
            );
            search(
                references,
                uri,
                file,
                cursor.to_child(rhs),
                pattern,
                false,
                exclude,
            );
        }
        NodeType::FormList { items } => {
            for item in items {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(item),
                    pattern,
                    false,
                    exclude,
                );
            }
        }
        NodeType::FormListItem { lhs: _, rhs } => {
            if let Some(rhs) = rhs {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(rhs),
                    pattern,
                    false,
                    exclude,
                );
            }
        }
        NodeType::SubList { items } => {
            for item in items {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(item),
                    pattern,
                    false,
                    exclude,
                );
            }
        }
        NodeType::SubListItem { lhs: _, rhs } => {
            if let Some(rhs) = rhs {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(rhs),
                    pattern,
                    false,
                    exclude,
                );
            }
        }
        NodeType::WhiteSpace => {}
        NodeType::File { exprs } => {
            for expr in exprs {
                search(
                    references,
                    uri,
                    file,
                    cursor.to_child(expr),
                    pattern,
                    false,
                    exclude,
                );
            }
        }
        NodeType::ErrorBoundary { node } => search(
            references,
            uri,
            file,
            cursor.to_child(node),
            pattern,
            false,
            exclude,
        ),
        NodeType::Empty(_) => {}
    }
}

fn matches_pattern(node: &Node, pattern: &Pattern) -> bool {
    match (pattern, node.node_type()) {
        (Pattern::Name(name), NodeType::Symbol { value })
        | (Pattern::Name(name), NodeType::LiteralString { value }) => value == name,
        (Pattern::Dots, NodeType::Symbol { value })
        | (Pattern::Dots, NodeType::LiteralString { value }) => value == "..." || is_dotn(value),
        (Pattern::NameSpace { package, name }, NodeType::NameSpace { lhs, rhs, .. }) => {
            matches_pattern(lhs, &Pattern::Name(package.clone()))
                && matches_pattern(rhs, &Pattern::Name(name.clone()))
        }
        (_, _) => false,
    }
}

// Check for ..1, ..2, etc.
fn is_dotn(x: &str) -> bool {
    x.strip_prefix("..")
        .is_some_and(|x| x.parse::<usize>().is_ok())
}

enum Pattern {
    Name(String),
    NameSpace { package: String, name: String },
    Dots,
}

impl Pattern {
    fn finder(&self) -> PatternFinder<'_> {
        match self {
            Pattern::Name(name) => {
                let finder = Finder::new(name);
                PatternFinder::Name(finder)
            }
            Pattern::NameSpace { package, name } => {
                let lhs = Finder::new(package);
                let rhs = Finder::new(name);
                PatternFinder::NameSpace(lhs, rhs)
            }
            Pattern::Dots => PatternFinder::Name(Finder::new("..")),
        }
    }
}

enum PatternFinder<'a> {
    Name(Finder<'a>),
    NameSpace(Finder<'a>, Finder<'a>),
}

impl<'a> PatternFinder<'a> {
    fn search(&self, haystack: &[u8]) -> bool {
        match self {
            PatternFinder::Name(finder) => finder.find(haystack).is_some(),
            PatternFinder::NameSpace(lhs, rhs) => {
                lhs.find(haystack).is_some() && rhs.find(haystack).is_some()
            }
        }
    }
}
