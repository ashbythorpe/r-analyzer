use std::process::Command;

use crate::server::Server;

use super::definition::{get_definition, Definition};
use anyhow::Result;
use html2md::parse_html;

pub fn hover(server: &Server, params: lsp_types::HoverParams) -> Result<Option<lsp_types::Hover>> {
    let context = server
        .file_context(&params.text_document_position_params.text_document.uri)
        .unwrap();

    let definition = match get_definition(
        server,
        &context,
        params.text_document_position_params.position.into(),
    )? {
        Some(definition) => definition,
        None => return Ok(None),
    };

    match definition {
        Definition::PackageSymbol { package, name } => {
            let documentation = get_documentation(&package, &name)?;

            match documentation {
                Some(doc) => Ok(Some(lsp_types::Hover {
                    contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: doc,
                    }),
                    range: None,
                })),
                None => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

fn get_documentation(package: &str, name: &str) -> Result<Option<String>> {
    let expr = format!(
        "
file <- utils::help(topic = '{name}', package = '{package}')

if (length(file) > 0) {{
    cat(tools::Rd2HTML(utils:::.getHelpFile(file), package = '{package}'))
}}
"
    );

    let result = Command::new("R").args(["--slave", "-e", &expr]).output()?;

    if !result.status.success() {
        anyhow::bail!(
            "Failed to get package documentation.\nStderr: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }

    let output = String::from_utf8(result.stdout)?;

    if output.is_empty() {
        Ok(None)
    } else {
        Ok(Some(parse_html(&output)))
    }
}
