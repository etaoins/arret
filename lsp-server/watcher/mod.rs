mod syntax;
pub use syntax::*;

use std::sync::Arc;

use crate::model::Document;

/// Trait for a loosely coupled component that watches document events
pub trait DocumentWatcher {
    /// Called when a document is opened with the specified initial contents
    fn did_open(&mut self, _url: &lsp_types::Url, _document: &Arc<Document>) {}

    /// Called when a document has changed with the updated contents
    fn did_change(&mut self, _url: &lsp_types::Url, _document: &Arc<Document>) {}

    /// Called when a document has closed
    fn did_close(&mut self, _url: &lsp_types::Url) {}
}
