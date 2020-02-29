use lsp_types;

pub fn handle(_request: lsp_types::InitializeParams) -> lsp_types::InitializeResult {
    lsp_types::InitializeResult {
        server_info: Some(lsp_types::ServerInfo {
            name: "arret-lsp-server".to_owned(),
            version: None,
        }),
        capabilities: lsp_types::ServerCapabilities {
            ..Default::default()
        },
    }
}
