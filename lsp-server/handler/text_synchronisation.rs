use std::sync::Arc;

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
            Document::new(Some(text_document.version), Arc::from(text_document.text)),
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

        for content_change in content_changes {
            if content_change.range.is_some() || content_change.range_length.is_some() {
                eprintln!(
                    "Received unexpected incremental update to document {}",
                    text_document.uri
                );
                continue;
            }

            state.documents.insert(
                text_document.uri.to_string(),
                Document::new(text_document.version, Arc::from(content_change.text)),
            );
        }
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
