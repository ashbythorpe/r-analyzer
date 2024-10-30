use std::{fs::File, io::Write, process::Command};

use anyhow::{anyhow, Ok, Result};
use lsp_types::Uri;

use crate::{
    cursor::{node_at_position, Cursor},
    file::SourceFile,
    grammar::{FilePosition, FileSpan, TokenType},
    nodes::{Node, NodeType},
    utils::{file_to_uri, iter_args, path_to_uri, split_assignment, Arg},
    Server,
};

pub fn go_to_definition(
    server: &Server,
    params: lsp_types::GotoDefinitionParams,
) -> Result<Option<lsp_types::Location>> {
    let uri = params.text_document_position_params.text_document.uri;
    let file = server.get_file(uri.clone())?;

    let position = params.text_document_position_params.position.into();

    let definition = get_definition(server, file, uri, position)?;

    match definition {
        Some(Definition::FileSymbol { uri, file, node }) => node_definition(file, uri, node),
        Some(Definition::PackageSymbol { package, name }) => {
            get_definition_in_package(server, package.as_str(), name.as_str())
        }
        Some(Definition::Param { function, arg }) => param_definition(server, &function, &arg),
        None => Ok(None),
    }
}

pub fn get_definition<'a>(
    server: &'a Server,
    file: &'a SourceFile,
    uri: Uri,
    position: FilePosition,
) -> Result<Option<Definition<'a>>> {
    let cursor = node_at_position(file, position);

    let token = file.token_at(position);
    let node = cursor.current();

    Ok(match *token.token_type() {
        TokenType::Symbol => get_symbol_token_definition(server, file, uri, cursor, node),
        TokenType::RawString | TokenType::String => {
            get_string_definition(server, file, uri, cursor, node)
        }
        TokenType::Infix => get_symbol_definition(server, file, uri, cursor, token.content(), true),
        TokenType::DoubleColon | TokenType::TripleColon => get_namespaced_definition(server, node),
        TokenType::Dots | TokenType::Dot => get_dots_definition(file, uri, cursor, node),
        TokenType::Placeholder => get_placeholder_definition(file, uri, cursor, node),
        _ => return Ok(None),
    })
}

fn param_definition(
    server: &Server,
    function: &Definition,
    arg: &str,
) -> Result<Option<lsp_types::Location>> {
    match function {
        Definition::FileSymbol { uri, file, node } => {
            if let Some((_lhs, rhs)) = split_assignment(node) {
                if let NodeType::Function { args, body: _ } = rhs.node_type() {
                    let arg = iter_args(args)
                        .find(|Arg { lhs, .. }| lhs.is_some_and(|lhs| node_matches(lhs, arg)));

                    if let Some(Arg { lhs: Some(lhs), .. }) = arg {
                        return node_definition(file, uri.clone(), lhs);
                    }
                }
            }
        }
        Definition::PackageSymbol { package, name } => {
            let output = get_definition_string(package, name)?;

            if !output.starts_with("function") {
                return Ok(None);
            }

            let start_index = match output.find('(') {
                Some(index) => index,
                None => return Ok(None),
            };

            let str = &output[start_index..];

            let bracket_index = match find_matching_bracket(str) {
                Some(index) => index,
                None => return Ok(None),
            };

            let str = &str[1..bracket_index - 1];

            let mut column = 0;

            for (line_number, line) in str.lines().enumerate() {
                for arg_str in line.split(',') {
                    let arg = arg_str.trim_start();

                    if arg.starts_with(format!("{} = ", arg).as_str()) {
                        let whitespace_len = arg.chars().take_while(|c| c.is_whitespace()).count();
                        return generate_function(
                            server,
                            package,
                            name,
                            &output,
                            Some(FileSpan::new(
                                line_number,
                                column + whitespace_len,
                                line_number,
                                column + whitespace_len + name.chars().count(),
                            )),
                        );
                    }

                    column += arg.chars().count() + 1;
                }

                column = 0;
            }
        }
        Definition::Param {
            function: _,
            arg: _,
        } => panic!("Recursive parameter definition"),
    }

    Ok(None)
}

fn find_matching_bracket(x: &str) -> Option<usize> {
    let mut count = 0;

    for (i, c) in x.chars().enumerate() {
        if c == '(' {
            count += 1;
        } else if c == ')' {
            count -= 1;
        }

        if count == 0 {
            return Some(i);
        }
    }

    None
}

pub enum Definition<'a> {
    FileSymbol {
        uri: Uri,
        file: &'a SourceFile,
        node: &'a Node,
    },
    PackageSymbol {
        package: String,
        name: String,
    },
    Param {
        function: Box<Definition<'a>>,
        arg: String,
    },
}

fn get_placeholder_definition<'a>(
    file: &'a SourceFile,
    current_uri: Uri,
    mut cursor: Cursor<'a>,
    node: &'a Node,
) -> Option<Definition<'a>> {
    cursor.go_to_parent().expect("Symbol must have a parent");

    let mut current = cursor.current();

    if let NodeType::SubListItem { lhs, rhs } = current.node_type() {
        // Placeholder must be used in a named argument
        if !(node.equals_option(rhs) && lhs.is_some()) {
            return None;
        }

        cursor.go_to_parent().expect("Symbol must have a parent");

        let current = cursor.current();

        if let NodeType::Binary { op, lhs, rhs } = current.node_type() {
            if matches!(op.token_type(), TokenType::Pipe) && current == rhs.as_ref() {
                return Some(Definition::FileSymbol {
                    uri: current_uri,
                    file,
                    node: lhs,
                });
            }
        }
    }

    let mut prev = node;
    let mut valid = false;

    while let NodeType::Extract { lhs, rhs: _ }
    | NodeType::Subset { lhs, args: _ }
    | NodeType::Index { lhs, args: _ } = current.node_type()
    {
        if lhs.as_ref() == prev {
            cursor.go_to_parent().expect("Symbol must have a parent");

            prev = current;
            current = cursor.current();
            valid = true;
        } else {
            return None;
        }
    }

    if !valid {
        return None;
    }

    if let NodeType::Binary { op, lhs, rhs } = current.node_type() {
        if matches!(op.token_type(), TokenType::Pipe) && current == rhs.as_ref() {
            return Some(Definition::FileSymbol {
                uri: current_uri,
                file,
                node: lhs,
            });
        }
    }

    None
}

fn get_dots_definition<'a>(
    file: &'a SourceFile,
    current_uri: Uri,
    mut cursor: Cursor<'a>,
    node: &'a Node,
) -> Option<Definition<'a>> {
    match node.node_type() {
        NodeType::Symbol { value: _ } => {}
        _ => panic!("Expected symbol"),
    };

    cursor.go_to_parent().expect("Symbol must have a parent");

    while cursor.go_to_parent().is_ok() {
        let parent = cursor.current();

        match parent.node_type() {
            NodeType::FormListItem { lhs, rhs: _ } => {
                if node == lhs.as_ref() && is_dots(file, node) {
                    return Some(Definition::FileSymbol {
                        uri: current_uri,
                        file,
                        node,
                    });
                }

                while cursor.go_to_previous_sibling().is_ok() {
                    let node = cursor.current();

                    if is_dots(file, node) {
                        return Some(Definition::FileSymbol {
                            uri: current_uri,
                            file,
                            node,
                        });
                    }
                }
            }
            NodeType::Function { args, body: _ } => {
                for Arg {
                    lhs,
                    rhs: _,
                    node: _,
                } in iter_args(args)
                {
                    if lhs.is_some_and(|x| is_dots(file, x)) {
                        return Some(Definition::FileSymbol {
                            uri: current_uri,
                            file,
                            node: lhs.unwrap(),
                        });
                    }
                }
            }
            _ => {}
        }
    }

    None
}

fn is_dots(file: &SourceFile, node: &Node) -> bool {
    let (start, end) = match node.text_span(file.get_tokens()) {
        Some(span) => span.positions(),
        None => return false,
    };

    file.token_at(start).token_type() == &TokenType::Dots
        && file.token_at(end).token_type() == &TokenType::Dots
}

fn get_symbol_token_definition<'a>(
    server: &'a Server,
    file: &'a SourceFile,
    current_uri: Uri,
    mut cursor: Cursor<'a>,
    node: &'a Node,
) -> Option<Definition<'a>> {
    let value = match node.node_type() {
        NodeType::Symbol { value } => value,
        _ => panic!("Expected symbol"),
    };

    cursor.go_to_parent().expect("Symbol must have a parent");

    let parent = cursor.current();

    let is_function = match parent.node_type() {
        NodeType::NameSpace {
            internal: _,
            lhs,
            rhs: _,
        } => {
            if node == lhs.as_ref() {
                return get_package_definition(server, file, node);
            } else {
                return get_namespaced_definition(server, parent);
            }
        }
        NodeType::Extract { lhs: _, rhs } => {
            if node == rhs.as_ref() {
                // Can't get definition of e.g. x${field}
                return None;
            } else {
                false
            }
        }
        NodeType::Call {
            function: _,
            args: _,
        } => {
            match cursor.parent().unwrap().node_type() {
                NodeType::Extract { lhs: _, rhs } if node == rhs.as_ref() => {
                    return None;
                }
                _ => {}
            }

            true
        }
        NodeType::FormListItem { lhs, rhs: _ } => {
            if node == lhs.as_ref() {
                return Some(Definition::FileSymbol {
                    uri: current_uri,
                    file,
                    node,
                });
            } else {
                cursor.go_to_parent().expect("Node must have a parent");
                false
            }
        }
        NodeType::SubListItem { lhs, rhs: _ } => {
            if Some(node) == lhs.as_ref().map(|x| x.as_ref()) {
                return get_parameter_definition(
                    server,
                    file,
                    current_uri,
                    cursor,
                    node_value(node).unwrap(),
                );
            } else {
                false
            }
        }
        _ => false,
    };

    cursor.go_to_child(node);

    get_symbol_definition(server, file, current_uri, cursor, value, is_function)
}

fn get_parameter_definition<'a>(
    server: &'a Server,
    file: &'a SourceFile,
    current_uri: Uri,
    mut cursor: Cursor<'a>,
    name: &str,
) -> Option<Definition<'a>> {
    cursor.go_to_parent().expect("Parameter must have a parent");
    cursor
        .go_to_parent()
        .expect("Parameter list must have a parent");

    let definition = match cursor.current().node_type() {
        NodeType::Call { function, args: _ } => match function.node_type() {
            NodeType::NameSpace {
                internal: _,
                lhs: _,
                rhs: _,
            } => get_namespaced_definition(server, function)?,
            NodeType::Symbol { value } | NodeType::LiteralString { value } => {
                get_symbol_definition(server, file, current_uri, cursor, value, true)?
            }
            _ => return None,
        },
        _ => return None,
    };

    Some(Definition::Param {
        function: Box::new(definition),
        arg: name.to_string(),
    })
}

fn get_string_definition<'a>(
    server: &'a Server,
    file: &'a SourceFile,
    current_uri: Uri,
    mut cursor: Cursor<'a>,
    node: &'a Node,
) -> Option<Definition<'a>> {
    let value = match node.node_type() {
        NodeType::LiteralString { value } => value,
        _ => panic!("Expected string"),
    };

    cursor
        .go_to_parent()
        .expect("String node must have a parent");

    let parent = cursor.current();

    let is_function = match parent.node_type() {
        NodeType::Binary {
            op: _,
            lhs: _,
            rhs: _,
        } => {
            if let Some((lhs, rhs)) = split_assignment(parent) {
                if node == lhs {
                    matches!(rhs.node_type(), NodeType::Function { .. })
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        NodeType::NameSpace {
            internal: _,
            lhs,
            rhs: _,
        } => {
            if node == lhs.as_ref() {
                return get_package_definition(server, file, node);
            } else {
                return get_namespaced_definition(server, parent);
            }
        }
        NodeType::Call { .. } => {
            match cursor.parent().unwrap().node_type() {
                NodeType::Extract { lhs: _, rhs } if node == rhs.as_ref() => {
                    return None;
                }
                _ => {}
            }

            true
        }
        NodeType::Subset { .. } => false,
        NodeType::Index { .. } => false,
        NodeType::Extract { lhs: _, rhs } => {
            if node == rhs.as_ref() {
                // Can't get definition of e.g. x${field}
                return None;
            } else {
                false
            }
        }
        NodeType::FormListItem { lhs, rhs: _ } => {
            if node == lhs.as_ref() {
                return Some(Definition::FileSymbol {
                    uri: current_uri,
                    file,
                    node,
                });
            } else {
                return None;
            }
        }
        NodeType::SubListItem { lhs, rhs: _ } => {
            if Some(node) == lhs.as_ref().map(|x| x.as_ref()) {
                return get_parameter_definition(
                    server,
                    file,
                    current_uri,
                    cursor,
                    node_value(node).unwrap(),
                );
            } else {
                return None;
            }
        }
        _ => return None,
    };

    cursor.go_to_child(node);

    get_symbol_definition(server, file, current_uri, cursor, value, is_function)
}

fn node_definition(
    file: &SourceFile,
    uri: Uri,
    node: &Node,
) -> Result<Option<lsp_types::Location>> {
    Ok(Some(lsp_types::Location {
        uri,
        range: node
            .text_span(file.get_tokens())
            .expect("Node must have a span")
            .into(),
    }))
}

fn get_symbol_definition<'a>(
    server: &'a Server,
    file: &'a SourceFile,
    current_uri: Uri,
    mut cursor: Cursor<'a>,
    name: &str,
    is_function: bool,
) -> Option<Definition<'a>> {
    while !cursor.is_top_level() {
        let mut definitions = Vec::new();
        while cursor.go_to_previous_sibling().is_ok() {
            let node = cursor.current();

            search(&mut definitions, node, name);
        }

        cursor = step_out(&mut definitions, cursor, name);

        let definition = definitions
            .iter()
            .rev()
            .find(|node| (!is_function) || definition_is_function(node))
            .or(definitions.last());

        if let Some(definition) = definition {
            return Some(Definition::FileSymbol {
                uri: current_uri,
                file,
                node: definition,
            });
        }
    }

    if let Some(symbol) = server.symbol_index().find_symbol(name) {
        let uri = path_to_uri(symbol.file()).ok()?;
        let file = server.get_file(uri.clone()).ok()?;

        return Some(Definition::FileSymbol {
            uri,
            file,
            node: symbol.get_node(server).ok()?,
        });
    }

    let package_index = server.package_index();

    let (package, _) = match package_index.find_name(name) {
        Some(x) => x,
        None => return None,
    };

    return Some(Definition::PackageSymbol {
        package: package.name().to_string(),
        name: name.to_string(),
    });
}

fn get_package_definition<'a>(_: &Server, _: &SourceFile, _: &Node) -> Option<Definition<'a>> {
    // TODO: Do we need this?
    None
}

fn definition_is_function(node: &Node) -> bool {
    let rhs = match node.node_type() {
        NodeType::Binary { op, lhs, rhs } => match op.token_type() {
            TokenType::LeftAssign | TokenType::Equals => rhs,
            TokenType::RightAssign => lhs,
            _ => return false,
        },
        _ => return false,
    };

    matches!(rhs.node_type(), NodeType::Function { .. })
}

fn search_args<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, name: &str) {
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

fn step_out<'a>(nodes: &mut Vec<&'a Node>, mut cursor: Cursor<'a>, name: &str) -> Cursor<'a> {
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
        } => search_condition(nodes, condition, name),
        NodeType::For {
            condition: _,
            expr: _,
        } => search_condition(nodes, node, name),
        NodeType::While {
            condition: _,
            expr: _,
        } => search_condition(nodes, node, name),
        NodeType::Function { args, body: _ } => {
            search_args(nodes, args, name);
        }
        NodeType::Call { function, args: _ } => {
            if node != function.as_ref() {
                search_args(nodes, function, name);
            }
        }
        NodeType::Subset { lhs, args: _ } => {
            if node != lhs.as_ref() {
                search_args(nodes, lhs, name);
            }
        }
        NodeType::Index { lhs, args: _ } => {
            if node != lhs.as_ref() {
                search_args(nodes, lhs, name);
            }
        }
        NodeType::ForCondition { lhs, rhs: _ } => match lhs.node_type() {
            NodeType::ForCondition { lhs: _, rhs } => {
                search(nodes, rhs, name);

                if node_matches(lhs, name) {
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

fn search<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, name: &str) {
    match node.node_type() {
        NodeType::PrefixCall { rhs } => search(nodes, rhs, name),
        NodeType::Parentheses { contents } => search(nodes, contents, name),
        NodeType::Braces { exprs } => exprs.iter().rev().for_each(|e| search(nodes, e, name)),
        NodeType::If {
            condition,
            consequent_expr,
            alternative_expr,
        } => {
            if let Some(alternative) = alternative_expr {
                search(nodes, alternative, name);
            }
            search(nodes, consequent_expr, name);
            search_condition(nodes, condition, name);
        }
        NodeType::For { condition, expr } => {
            search(nodes, expr, name);
            search_condition(nodes, condition, name);
        }
        NodeType::While { condition, expr } => {
            search(nodes, expr, name);
            search_condition(nodes, condition, name);
        }
        NodeType::Repeat { expr } => search(nodes, expr, name),
        NodeType::Call { function: x, args } => {
            search_sublist(nodes, args, name);

            search(nodes, x, name);
        }
        NodeType::Subset { lhs, args } => {
            search_sublist(nodes, args, name);

            search(nodes, lhs, name);
        }
        NodeType::Index { lhs, args } => {
            search_sublist(nodes, args, name);

            search(nodes, lhs, name);
        }
        NodeType::Extract { lhs, rhs: _ } => {
            search(nodes, lhs, name);
        }
        NodeType::Binary { lhs, rhs, op } => match op.token_type() {
            TokenType::LeftAssign | TokenType::Equals => {
                if node_matches(lhs, name) {
                    nodes.push(node);
                }
            }
            TokenType::RightAssign => {
                if node_matches(rhs, name) {
                    nodes.push(node);
                }
            }
            _ => {
                search(nodes, rhs, name);
                search(nodes, lhs, name);
            }
        },
        _ => (),
    }
}

fn node_matches(node: &Node, name: &str) -> bool {
    match node.node_type() {
        NodeType::Symbol { value } => value == name,
        NodeType::LiteralString { value } => value == name,
        _ => false,
    }
}

fn search_condition<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, name: &str) {
    match node.node_type() {
        NodeType::Condition { expr } => search(nodes, expr, name),
        NodeType::ForCondition { lhs: _, rhs } => search(nodes, rhs, name),
        _ => panic!("Expected condition"),
    }
}

fn search_sublist<'a>(nodes: &mut Vec<&'a Node>, node: &'a Node, name: &str) {
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

fn get_namespaced_definition<'a>(server: &Server, node: &Node) -> Option<Definition<'a>> {
    let (internal, lhs, rhs) = match node.node_type() {
        NodeType::NameSpace { internal, lhs, rhs } => (internal, lhs.as_ref(), rhs.as_ref()),
        _ => panic!("Expected namespace"),
    };

    let lhs_str = match node_value(lhs) {
        Some(x) => x,
        None => return None,
    };

    let rhs_str = match node_value(rhs) {
        Some(x) => x,
        None => return None,
    };

    match server
        .package_index()
        .get_symbol(lhs_str, rhs_str, *internal)
    {
        Some(x) => x,
        None => return None,
    };

    Some(Definition::PackageSymbol {
        package: lhs_str.to_string(),
        name: rhs_str.to_string(),
    })
}

fn node_value(node: &Node) -> Option<&str> {
    match node.node_type() {
        NodeType::Symbol { value } => Some(value),
        NodeType::LiteralString { value } => Some(value),
        _ => None,
    }
}

fn get_definition_in_package(
    server: &Server,
    package: &str,
    name: &str,
) -> Result<Option<lsp_types::Location>> {
    let output = get_definition_string(package, name)?;

    generate_function(server, package, name, &output, None)
}

fn get_definition_string(package: &str, name: &str) -> Result<String> {
    let full_name = format!("{}:::`{}`", package, name);

    let result = Command::new("R")
        .args([
            "--slave",
            "-e",
            &format!(
                "if (is.primitive({x})) print({x}) else cat(deparse({x}), sep = '\n')",
                x = full_name,
            ),
        ])
        .output()?;

    if !result.status.success() {
        anyhow::bail!("Failed to get package definition")
    }

    Ok(String::from_utf8(result.stdout)?)
}

fn generate_function(
    server: &Server,
    package: &str,
    name: &str,
    definition: &str,
    span: Option<FileSpan>,
) -> Result<Option<lsp_types::Location>> {
    let span = span
        .map(|x| x.shift(2, 0))
        .unwrap_or(FileSpan::new(2, 0, 2, 0));

    let temp_dir = server.temp_dir();

    let path = temp_dir.path().join(format!("{}::{}.R", package, name));

    let mut file = File::create(&path)?;

    let contents = format!(
        "# Generated by function body. Editing this file has no effect.\n\n{}",
        definition
    );

    file.write_all(contents.as_bytes())?;

    let path_str = path.to_str().ok_or(anyhow!("Invalid path"))?;

    let uri = file_to_uri(path_str)?;

    Ok(Some(lsp_types::Location {
        uri,
        range: span.into(),
    }))
}
