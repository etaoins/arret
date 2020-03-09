use lsp_types;

pub fn server_capabilities() -> lsp_types::ServerCapabilities {
    lsp_types::ServerCapabilities {
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            lsp_types::TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(lsp_types::TextDocumentSyncKind::Incremental),
                ..Default::default()
            },
        )),
        workspace: Some(lsp_types::WorkspaceCapability {
            workspace_folders: Some(lsp_types::WorkspaceFolderCapability {
                supported: Some(true),
                change_notifications: Some(
                    lsp_types::WorkspaceFolderCapabilityChangeNotifications::Bool(true),
                ),
            }),
        }),
        ..Default::default()
    }
}
