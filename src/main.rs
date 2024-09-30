use core::panic;

use server::Server;

use anyhow::Result;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId};
use lsp_types::notification::{self};
use lsp_types::request as lsp_request;
use lsp_types::{
    InitializeParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use ropey::Rope;
use url::Url;

#[macro_use]
mod macros;

mod char_traverser;
mod cursor;
pub mod file;
mod format;
mod grammar;
mod handlers;
mod lexer;
pub mod nodes;
mod parser;
mod server;

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
    let mut server = Server::new();

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

                    server.add_file(document.uri, Rope::from(document.text));
                }
                "textDocument/didChange" => {
                    let params =
                        cast_notification::<notification::DidChangeTextDocument>(notification)?;

                    let document = params.text_document;
                    let url = Url::parse(document.uri.as_str())?;
                    let path = match url.to_file_path() {
                        Ok(x) => x,
                        Err(_) => {
                            return Err(anyhow::anyhow!("Invalid file path: {:?}", url));
                        }
                    };

                    let absolute_path = camino::absolute_utf8(&path)?;
                    server.update_file(absolute_path, params.content_changes)?
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
