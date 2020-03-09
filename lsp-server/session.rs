use std::collections::HashMap;
use std::sync::Arc;

use lsp_types;
use tokio::sync::mpsc;

use crate::capabilities::server_capabilities;
use crate::handler;
use crate::json_rpc::{ClientMessage, ErrorCode, Response, ServerMessage};
use crate::model::{Document, Workspace};
use crate::transport::Connection;
use crate::watcher::SyntaxWatcher;

pub struct State {
    pub documents: HashMap<String, Arc<Document>>,
    pub workspaces: HashMap<String, Arc<Workspace>>,
    pub syntax_watcher: SyntaxWatcher,
}

impl State {
    fn new(
        outgoing: mpsc::Sender<ServerMessage>,
        initialize_params: lsp_types::InitializeParams,
    ) -> State {
        let initial_workspaces = initialize_params
            .workspace_folders
            .map(|workspace_folders| {
                workspace_folders
                    .into_iter()
                    .map(|workspace_folder| {
                        (
                            workspace_folder.uri.to_string(),
                            Arc::new(Workspace::new(workspace_folder.name)),
                        )
                    })
                    .collect()
            })
            .unwrap_or_else(HashMap::new);

        State {
            documents: HashMap::new(),
            workspaces: initial_workspaces,
            syntax_watcher: SyntaxWatcher::new(outgoing),
        }
    }

    async fn shutdown(self) {
        self.syntax_watcher.shutdown().await;
    }
}

pub fn create_initialize_response() -> lsp_types::InitializeResult {
    lsp_types::InitializeResult {
        server_info: Some(lsp_types::ServerInfo {
            name: "arret-lsp-server".to_owned(),
            version: None,
        }),
        capabilities: server_capabilities(),
    }
}

/// Runs a session loop against the provided connection
///
/// On a clean exit (`shutdown` followed by `exit`) this will return `Ok`, otherwise it will return
/// `Err`.
pub async fn run(connection: Connection) -> Result<(), ()> {
    let Connection {
        mut incoming,
        mut outgoing,
    } = connection;

    /// Receives an incoming message or returns `Err` if the receive channel is closed
    ///
    /// This will cause us to exit uncleanly if our connection closes unexpectedly.
    macro_rules! recv_or_return_err {
        () => {
            match incoming.recv().await {
                Some(incoming_message) => incoming_message,
                None => {
                    eprintln!("Connection unexpectedly closed");
                    return Err(());
                }
            }
        };
    }

    /// Sends the outgoing message or returns `Err` if the send channel is closed
    macro_rules! send_or_return_err {
        ($outgoing_message:expr) => {
            if outgoing.send($outgoing_message.into()).await.is_err() {
                eprintln!("Connection unexpectedly closed");
                return Err(());
            }
        };
    }

    // Wait for initialize
    let initialize_request = loop {
        match recv_or_return_err!() {
            ClientMessage::Notification(notification) => {
                if notification.method == "exit" {
                    // Unclean exit
                    return Err(());
                }
            }
            ClientMessage::Request(request) if request.method.as_str() == "initialize" => {
                break request;
            }
            ClientMessage::Request(request) => {
                send_or_return_err!(Response::new_err(
                    request.id,
                    ErrorCode::ServerNotInitialized,
                    "Server not initialized"
                ));
            }
        }
    };

    let params: lsp_types::InitializeParams = serde_json::from_value(initialize_request.params)
        .expect("Could not parse initialize request params");

    let mut state = State::new(outgoing.clone(), params);

    let initialize_response = create_initialize_response();
    send_or_return_err!(Response::new_ok(
        initialize_request.id.clone(),
        initialize_response
    ));

    // Process normal messages until we receive a shutdown request
    loop {
        match recv_or_return_err!() {
            ClientMessage::Notification(notification) if notification.method == "initialized" => {
                // Nothing do to
            }
            ClientMessage::Notification(notification) if notification.method == "exit" => {
                // Tear down our state or we'll likely to panic if there are concurrent operationss
                state.shutdown().await;

                // Unclean exit
                return Err(());
            }
            ClientMessage::Notification(notification) => {
                handler::handle_non_lifecycle_notification(&mut state, notification);
            }
            ClientMessage::Request(request) if request.method == "shutdown" => {
                send_or_return_err!(Response::new_ok(request.id, ()));
                break;
            }
            ClientMessage::Request(request) => {
                send_or_return_err!(handler::handle_non_lifecycle_request(&mut state, request));
            }
        }
    }

    // Cleanly shutdown our state
    state.shutdown().await;

    // Wait for exit
    loop {
        match recv_or_return_err!() {
            ClientMessage::Notification(notification) => {
                if notification.method == "exit" {
                    return Ok(());
                }
            }
            ClientMessage::Request(request) => {
                send_or_return_err!(Response::new_err(
                    request.id,
                    ErrorCode::InvalidRequest,
                    "Shutting down"
                ));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::future::Future;

    use tokio::sync::mpsc;

    use crate::json_rpc::{Notification, Request, RequestId, ServerMessage};

    struct TestSession<F>
    where
        F: Future<Output = Result<(), ()>>,
    {
        outgoing: mpsc::Receiver<ServerMessage>,
        incoming: mpsc::Sender<ClientMessage>,
        exit_future: F,
    }

    fn run_test_session() -> TestSession<impl Future<Output = Result<(), ()>>> {
        let (send_outgoing, recv_outgoing) = mpsc::channel::<ServerMessage>(4);
        let (send_incoming, recv_incoming) = mpsc::channel::<ClientMessage>(4);

        let session = run(Connection {
            outgoing: send_outgoing,
            incoming: recv_incoming,
        });

        TestSession {
            outgoing: recv_outgoing,
            incoming: send_incoming,
            exit_future: session,
        }
    }

    fn expect_response(server_message: ServerMessage) -> Response {
        if let ServerMessage::Response(response) = server_message {
            response
        } else {
            panic!("Expected response, got {:?}", server_message);
        }
    }

    #[allow(deprecated)]
    #[tokio::test]
    async fn test_clean_lifecycle() {
        let TestSession {
            mut outgoing,
            mut incoming,
            exit_future,
        } = run_test_session();

        tokio::spawn(async move {
            // We should return an error for messages before initialization
            incoming
                .send(Request::new(123.into(), "shutdown", ()).into())
                .await
                .unwrap();

            let response = expect_response(outgoing.recv().await.unwrap());

            assert_eq!(
                Response::new_err(
                    123.into(),
                    ErrorCode::ServerNotInitialized,
                    "Server not initialized"
                ),
                response,
            );

            // Now initialize
            let initialize_params = lsp_types::InitializeParams {
                process_id: None,
                root_path: None,
                root_uri: None,
                initialization_options: None,
                capabilities: Default::default(),
                trace: None,
                workspace_folders: None,
                client_info: None,
            };

            incoming
                .send(Request::new("123".to_owned().into(), "initialize", initialize_params).into())
                .await
                .unwrap();

            let response = expect_response(outgoing.recv().await.unwrap());

            // Don't assert the exact body
            assert_eq!(response.id, RequestId::from("123".to_owned()));
            assert!(response.error.is_none());

            // Send initialized notification
            incoming
                .send(Notification::new("initialized", ()).into())
                .await
                .unwrap();

            // Now shutdown for real
            incoming
                .send(Request::new(456.into(), "shutdown", ()).into())
                .await
                .unwrap();

            let response = expect_response(outgoing.recv().await.unwrap());

            assert_eq!(Response::new_ok(456.into(), ()), response,);

            // We should return an error on duplicate shutdown
            incoming
                .send(Request::new("456".to_owned().into(), "shutdown", ()).into())
                .await
                .unwrap();

            let response = expect_response(outgoing.recv().await.unwrap());

            assert_eq!(
                Response::new_err(
                    "456".to_owned().into(),
                    ErrorCode::InvalidRequest,
                    "Shutting down"
                ),
                response,
            );

            // And send exit notification
            incoming
                .send(Notification::new("exit", ()).into())
                .await
                .unwrap();
        });

        // This should be considered a clean exit
        assert!(exit_future.await.is_ok());
    }
}
