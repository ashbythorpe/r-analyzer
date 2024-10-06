use core::panic;

use handlers::document_symbols::document_symbols;
use handlers::{document_symbols, expand_selection};
use server::Server;

use anyhow::Result;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{self};
use lsp_types::{request as lsp_request, WorkspaceEdit};
use lsp_types::{
    InitializeParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use ropey::Rope;

#[macro_use]
mod macros;

mod char_traverser;
mod cursor;
mod description;
pub mod file;
mod format;
mod grammar;
mod handlers;
mod lexer;
pub mod nodes;
mod package_index;
mod parser;
mod server;
mod utils;

fn main() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilites = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })
    .unwrap();

    let initialization_params: InitializeParams = match connection.initialize(server_capabilites) {
        Ok(x) => serde_json::from_value(x)?,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }

            return Err(e.into());
        }
    };

    main_loop(connection, initialization_params)?;

    Ok(())
}

fn main_loop(connection: Connection, params: InitializeParams) -> Result<()> {
    let mut server = Server::initialize(params)?;

    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(());
                }

                match request.method.as_str() {
                    "shutdown" => {
                        return Ok(());
                    }
                    "textDocument/selectionRange" => {
                        let (id, params) =
                            cast_request::<lsp_request::SelectionRangeRequest>(request)?;

                        let result = expand_selection::selection_range(&server, params)?;
                        let response = Response::new_ok(id, result);

                        connection.sender.send(Message::Response(response))?;
                    }
                    "textDocument/documentSymbol" => {
                        let (id, params) =
                            cast_request::<lsp_request::DocumentSymbolRequest>(request)?;

                        let result = document_symbols(&server, params)?;
                        let response = Response::new_ok(id, result);

                        connection.sender.send(Message::Response(response))?;
                    }
                    _ => {
                        return Err(anyhow::anyhow!("Unexpected request: {:?}", request));
                    }
                }
            }
            Message::Response(x) => {
                return Err(anyhow::anyhow!("Unexpected response: {:?}", x));
            }
            Message::Notification(notification) => match notification.method.as_str() {
                "textDocument/didOpen" => {
                    let params =
                        cast_notification::<notification::DidOpenTextDocument>(notification)?;

                    let document = params.text_document;

                    server.add_file(document.uri, Rope::from(document.text))?;
                }
                "textDocument/didChange" => {
                    let params =
                        cast_notification::<notification::DidChangeTextDocument>(notification)?;

                    server.update_file(params.text_document.uri, params.content_changes)?
                }
                "textDocument/didClose" => {
                    let params =
                        cast_notification::<notification::DidCloseTextDocument>(notification)?;

                    server.remove_file(params.text_document.uri)?;
                }
                _ => {
                    return Err(anyhow::anyhow!(
                        "Unexpected notification: {:?}",
                        notification
                    ));
                }
            },
        }
    }

    Ok(())
}

fn cast_request<R>(request: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    match request.extract(R::METHOD) {
        Err(ExtractError::MethodMismatch(_)) => panic!("Method mismatch"),
        x => x,
    }
}

fn cast_notification<R>(x: Notification) -> Result<R::Params, ExtractError<Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    match x.extract(R::METHOD) {
        Err(ExtractError::MethodMismatch(_)) => panic!("Method mismatch"),
        x => x,
    }
}
