use std::sync::Arc;

use lsp_types;

use crate::handler::SyncNotificationHandler;
use crate::model::Workspace;
use crate::session::State;

pub struct DidChangeWorkspaceFoldersHandler;

impl SyncNotificationHandler for DidChangeWorkspaceFoldersHandler {
    type Notification = lsp_types::notification::DidChangeWorkspaceFolders;

    fn handle(state: &mut State, params: lsp_types::DidChangeWorkspaceFoldersParams) {
        let lsp_types::DidChangeWorkspaceFoldersParams { event } = params;

        for added in event.added {
            state
                .workspaces
                .insert(added.uri.to_string(), Arc::new(Workspace::new(added.name)));
        }

        for removed in event.removed {
            state.workspaces.remove(removed.uri.as_str());
        }
    }
}
