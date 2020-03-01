use std::collections::HashMap;
use std::sync::Arc;

use futures::future::join_all;
use lsp_types;
use lsp_types::notification::Notification as _;
use tokio::sync::{mpsc, watch};
use tokio::task;

use arret_syntax::parser::data_from_str;

use crate::json_rpc::{Notification, ServerMessage};
use crate::model::Document;
use crate::watcher::DocumentWatcher;

fn syntax_diagnostics_for_document(document: &Document) -> Vec<lsp_types::Diagnostic> {
    match data_from_str(None, document.text()) {
        Ok(_) => vec![],
        Err(error) => vec![lsp_types::Diagnostic {
            range: lsp_types::Range {
                start: document.offset_to_position(error.span().start().to_usize()),
                end: document.offset_to_position(error.span().end().to_usize()),
            },
            severity: Some(lsp_types::DiagnosticSeverity::Error),
            message: error.kind().message(),
            source: Some("arret-syntax".to_owned()),
            ..Default::default()
        }],
    }
}

struct DocumentTask {
    send_change: watch::Sender<Arc<Document>>,
    join_handle: task::JoinHandle<()>,
}

impl DocumentTask {
    pub fn new(
        mut outgoing: mpsc::Sender<ServerMessage>,
        url: lsp_types::Url,
        document: Arc<Document>,
    ) -> DocumentTask {
        let (send_change, mut receive_change) = watch::channel(document);

        let join_handle = tokio::spawn(async move {
            while let Some(document) = receive_change.recv().await {
                let diagnostics = syntax_diagnostics_for_document(&document);

                if outgoing
                    .send(
                        Notification::new(
                            lsp_types::notification::PublishDiagnostics::METHOD,
                            lsp_types::PublishDiagnosticsParams {
                                uri: url.clone(),
                                version: document.version(),
                                diagnostics,
                            },
                        )
                        .into(),
                    )
                    .await
                    .is_err()
                {
                    break;
                }
            }
        });

        DocumentTask {
            send_change,
            join_handle,
        }
    }

    fn did_change(&self, document: Arc<Document>) {
        self.send_change
            .broadcast(document)
            .expect("Could not send change to document syntax task");
    }

    async fn shutdown(self) {
        drop(self.send_change);
        self.join_handle
            .await
            .expect("Document syntax task panicked");
    }
}

pub struct SyntaxWatcher {
    outgoing: mpsc::Sender<ServerMessage>,
    document_tasks: HashMap<String, DocumentTask>,
}

impl SyntaxWatcher {
    pub fn new(outgoing: mpsc::Sender<ServerMessage>) -> SyntaxWatcher {
        SyntaxWatcher {
            outgoing,
            document_tasks: HashMap::new(),
        }
    }

    pub async fn shutdown(self) {
        let document_task_futures = self
            .document_tasks
            .into_iter()
            .map(|(_, task)| task.shutdown());

        join_all(document_task_futures).await;
    }
}

impl DocumentWatcher for SyntaxWatcher {
    fn did_open(&mut self, url: &lsp_types::Url, document: &Arc<Document>) {
        self.document_tasks.insert(
            url.to_string(),
            DocumentTask::new(self.outgoing.clone(), url.clone(), Arc::clone(document)),
        );
    }

    fn did_change(&mut self, url: &lsp_types::Url, document: &Arc<Document>) {
        if let Some(document_task) = self.document_tasks.get(url.as_str()) {
            document_task.did_change(Arc::clone(document));
        }
    }

    fn did_close(&mut self, url: &lsp_types::Url) {
        self.document_tasks.remove(url.as_str());
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn correct_document_diagnostics() {
        let doc = Document::new(None, "('hello-world)".to_owned());
        let diags = syntax_diagnostics_for_document(&doc);

        assert_eq!(Vec::<lsp_types::Diagnostic>::new(), diags);
    }

    #[test]
    fn missing_delimiter_diagnostics() {
        let doc = Document::new(None, "('hello-world".to_owned());
        let diags = syntax_diagnostics_for_document(&doc);

        assert_eq!(
            vec![lsp_types::Diagnostic {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 13,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 13,
                    }
                },
                severity: Some(lsp_types::DiagnosticSeverity::Error),
                message: "unexpected end of file while parsing list".into(),
                source: Some("arret-syntax".to_owned()),
                ..Default::default()
            }],
            diags
        );
    }

    #[test]
    fn unsupported_character_diagnostics() {
        let doc = Document::new(None, "\\newline \\madeup".to_owned());
        let diags = syntax_diagnostics_for_document(&doc);

        assert_eq!(
            vec![lsp_types::Diagnostic {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 10,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 16,
                    }
                },
                severity: Some(lsp_types::DiagnosticSeverity::Error),
                message: "unsupported character".into(),
                source: Some("arret-syntax".to_owned()),
                ..Default::default()
            }],
            diags
        );
    }
}
