mod text_synchronisation;
use text_synchronisation::*;

mod workspace;
use workspace::*;

use lsp_types::notification::Notification as LspNotification;

use crate::json_rpc::{ErrorCode, Notification, Request, Response};
use crate::session::State;

/// Trait for handling notifications
pub trait SyncNotificationHandler {
    type Notification: LspNotification;

    fn handle(state: &mut State, params: <Self::Notification as LspNotification>::Params);
}

macro_rules! build_notification_dispatcher {
    ($name:ident, { $( $sync_handler:ty ),* }) => {
        pub fn $name(state: &mut State, notification: Notification) {
            match notification.method.as_str() {
                $(
                    <$sync_handler as SyncNotificationHandler>::Notification::METHOD => {
                        let params = serde_json::from_value(notification.params)
                            .expect("Could not parse notification params");

                        <$sync_handler as SyncNotificationHandler>::handle(state, params);
                    }
                )*,
                other => {
                    // Allow optional notifications
                    if !other.starts_with("$/") {
                        eprintln!("Unexpected notification method '{}'", notification.method);
                    }
                }
            }
        }
    };
}

build_notification_dispatcher!(handle_non_lifecycle_notification, {
    DidOpenTextDocumentHandler,
    DidChangeTextDocumentHandler,
    DidCloseTextDocumentHandler,
    DidChangeWorkspaceFoldersHandler
});

pub fn handle_non_lifecycle_request(_state: &mut State, request: Request) -> Response {
    // We only support lifecycle requests at the moment
    Response::new_err(request.id, ErrorCode::MethodNotFound, "Method not found")
}
