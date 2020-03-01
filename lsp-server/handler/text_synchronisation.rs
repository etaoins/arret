use std::sync::Arc;

use lsp_types;

use crate::handler::SyncNotificationHandler;
use crate::model::Document;
use crate::session::State;
use crate::watcher::DocumentWatcher;

pub struct DidOpenTextDocumentHandler;

impl SyncNotificationHandler for DidOpenTextDocumentHandler {
    type Notification = lsp_types::notification::DidOpenTextDocument;

    fn handle(state: &mut State, params: lsp_types::DidOpenTextDocumentParams) {
        let text_document = params.text_document;

        let document = Arc::new(Document::new(
            Some(text_document.version),
            text_document.text,
        ));

        state.syntax_watcher.did_open(&text_document.uri, &document);

        state
            .documents
            .insert(text_document.uri.to_string(), document);
    }
}

pub struct DidChangeTextDocumentHandler;

impl SyncNotificationHandler for DidChangeTextDocumentHandler {
    type Notification = lsp_types::notification::DidChangeTextDocument;

    fn handle(state: &mut State, params: lsp_types::DidChangeTextDocumentParams) {
        let lsp_types::DidChangeTextDocumentParams {
            text_document,
            content_changes,
        } = params;

        let orig_document =
            if let Some(document) = state.documents.remove(text_document.uri.as_str()) {
                document
            } else {
                eprintln!(
                    "Received change notification for unknown document {}",
                    text_document.uri
                );
                return;
            };

        let new_document =
            content_changes
                .into_iter()
                .fold(
                    orig_document,
                    |prev_document, content_change| match content_change.range {
                        Some(range) => {
                            match prev_document.with_range_edit(
                                text_document.version,
                                range,
                                &content_change.text,
                            ) {
                                Ok(new_document) => Arc::new(new_document),
                                Err(()) => {
                                    eprintln!(
                                        "Could not find range to replace in {}",
                                        text_document.uri
                                    );

                                    prev_document
                                }
                            }
                        }
                        None => Arc::new(Document::new(text_document.version, content_change.text)),
                    },
                );

        state
            .syntax_watcher
            .did_change(&text_document.uri, &new_document);

        state
            .documents
            .insert(text_document.uri.to_string(), new_document);
    }
}

pub struct DidCloseTextDocumentHandler;

impl SyncNotificationHandler for DidCloseTextDocumentHandler {
    type Notification = lsp_types::notification::DidCloseTextDocument;

    fn handle(state: &mut State, params: lsp_types::DidCloseTextDocumentParams) {
        let text_document = params.text_document;

        state.documents.remove(text_document.uri.as_str());
        state.syntax_watcher.did_close(&text_document.uri);
    }
}
