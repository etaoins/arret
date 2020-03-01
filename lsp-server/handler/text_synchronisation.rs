use lsp_types;

use crate::handler::SyncNotificationHandler;
use crate::model::Document;
use crate::session::State;

pub struct DidOpenTextDocumentHandler;

impl SyncNotificationHandler for DidOpenTextDocumentHandler {
    type Notification = lsp_types::notification::DidOpenTextDocument;

    fn handle(state: &mut State, params: lsp_types::DidOpenTextDocumentParams) {
        let text_document = params.text_document;

        state.documents.insert(
            text_document.uri.to_string(),
            Document::new(Some(text_document.version), text_document.text),
        );
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
                                Ok(new_document) => new_document,
                                Err(()) => {
                                    eprintln!(
                                        "Could not find range to replace in {}",
                                        text_document.uri
                                    );

                                    prev_document
                                }
                            }
                        }
                        None => Document::new(text_document.version, content_change.text),
                    },
                );

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
    }
}
