use core::panic;

use handlers::completion::propose_completions;
use handlers::definition::go_to_definition;
use handlers::document_symbols::document_symbols;
use handlers::expand_selection;
use handlers::hover::hover;
use handlers::references::find_references;
use log::info;
use log::LevelFilter;
use lsp_types::OneOf;
use server::Server;

use anyhow::Result;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification;
use lsp_types::request as lsp_request;
use lsp_types::{
    InitializeParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use ropey::Rope;
use simplelog::ColorChoice;
use simplelog::Config;
use simplelog::TermLogger;
use simplelog::TerminalMode;

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
mod symbol_index;
mod utils;
mod workspace;

fn main() -> Result<()> {
    TermLogger::init(
        LevelFilter::Info,
        Config::default(),
        TerminalMode::Stderr,
        ColorChoice::Auto,
    )?;

    let (connection, io_threads) = Connection::stdio();

    let server_capabilites = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: OneOf::Left(true).into(),
        references_provider: OneOf::Left(true).into(),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        selection_range_provider: Some(true.into()),
        document_symbol_provider: Some(OneOf::Left(true)),
        completion_provider: Some(lsp_types::CompletionOptions {
            ..Default::default()
        }),
        ..Default::default()
    })
    .unwrap();

    info!("Waiting for connection to initialize");

    let initialization_params: InitializeParams = match connection.initialize(server_capabilites) {
        Ok(x) => serde_json::from_value(x)?,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }

            return Err(e.into());
        }
    };

    info!("Connection initialized");

    let position_encodings = initialization_params
        .capabilities
        .general
        .as_ref()
        .and_then(|x| x.position_encodings.as_ref());

    info!("Position encoding: {:?}", position_encodings);

    main_loop(connection, initialization_params)
}

fn main_loop(connection: Connection, params: InitializeParams) -> Result<()> {
    info!("Initializing server");

    let mut server = Server::initialize(params)?;

    info!("Server initialized");

    for message in &connection.receiver {
        info!("Got message");
        info!("{:#?}", message);
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
                    "textDocument/definition" => {
                        info!("textDocument/definition");
                        let (id, params) = cast_request::<lsp_request::GotoDefinition>(request)?;

                        info!("Params: {:#?}", params);

                        let result = go_to_definition(&server, params)?;

                        info!("textDocument/definition result: {:#?}", result);
                        let response = Response::new_ok(id, result);

                        connection.sender.send(Message::Response(response))?;
                    }
                    "textDocument/references" => {
                        let (id, params) = cast_request::<lsp_request::References>(request)?;

                        let result = find_references(&server, params)?;
                        let response = Response::new_ok(id, result);

                        connection.sender.send(Message::Response(response))?;
                    }
                    "textDocument/hover" => {
                        let (id, params) = cast_request::<lsp_request::HoverRequest>(request)?;

                        let result = hover(&server, params)?;
                        let response = Response::new_ok(id, result);

                        connection.sender.send(Message::Response(response))?;
                    }
                    "textDocument/completion" => {
                        let (id, params) = cast_request::<lsp_request::Completion>(request)?;

                        let result = propose_completions(&server, params)?;
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

                    info!("Opening file: {:?}", params);

                    let document = params.text_document;

                    server.add_file(&document.uri, Rope::from(document.text))?;
                }
                "textDocument/didChange" => {
                    let params =
                        cast_notification::<notification::DidChangeTextDocument>(notification)?;

                    server.update_file(params.text_document.uri, params.content_changes)?
                }
                "textDocument/didClose" => {
                    let params =
                        cast_notification::<notification::DidCloseTextDocument>(notification)?;

                    server.remove_file(&params.text_document.uri)?;
                }
                "textDocument/didSave" => {}
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
