use crate::json_rpc::{ClientMessage, ErrorCode, Request, Response};
use crate::messages;
use crate::transport::Connection;
use lsp_types;
use serde_json;

fn handle_non_lifecycle_request(request: Request) -> Response {
    Response::new_err(request.id, ErrorCode::MethodNotFound, "Method not found")
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

    /// Sends the outgoing message or returns `Err` if the send channel is closed
    ///
    /// This will cause us to exit uncleanly if our connection closes unexpectedly.
    macro_rules! send_or_return_err {
        ($outgoing_message:expr) => {
            if outgoing.send($outgoing_message.into()).await.is_err() {
                eprintln!("Connection unexpectedly closed");
                return Err(());
            }
        };
    }

    // Wait for initialize
    while let Some(incoming_message) = incoming.recv().await {
        match incoming_message {
            ClientMessage::Notification(notification) => {
                if notification.method == "exit" {
                    // Unclean exit
                    return Err(());
                }
            }
            ClientMessage::Request(request) if request.method.as_str() == "initialize" => {
                let initialize_params: lsp_types::InitializeParams =
                    serde_json::from_value(request.params)
                        .expect("Could not parse initialize params");

                let response = messages::initialize::handle(initialize_params);
                send_or_return_err!(Response::new_ok(request.id, response));

                break;
            }
            ClientMessage::Request(request) => {
                send_or_return_err!(Response::new_err(
                    request.id,
                    ErrorCode::ServerNotInitialized,
                    "Server not initialized"
                ));
            }
        }
    }

    // Process normal messages until we receive a shutdown request
    while let Some(incoming_message) = incoming.recv().await {
        match incoming_message {
            ClientMessage::Notification(notification) => {
                if notification.method == "exit" {
                    // Unclean exit
                    return Err(());
                }
            }
            ClientMessage::Request(request) if request.method == "shutdown" => {
                send_or_return_err!(Response::new_ok(request.id, ()));
                break;
            }
            ClientMessage::Request(request) => {
                send_or_return_err!(handle_non_lifecycle_request(request));
            }
        }
    }

    // Wait for exit
    while let Some(incoming_message) = incoming.recv().await {
        match incoming_message {
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

    // Receiver channel unexpectedly closed
    Err(())
}

#[cfg(test)]
mod test {
    use super::*;

    use futures::future::BoxFuture;
    use futures::prelude::*;
    use tokio::sync::mpsc;

    use crate::json_rpc::{Notification, RequestId, ServerMessage};

    struct TestSession {
        outgoing: mpsc::Receiver<ServerMessage>,
        incoming: mpsc::Sender<ClientMessage>,
        exit_future: BoxFuture<'static, Result<(), ()>>,
    }

    fn run_test_session() -> TestSession {
        let (send_outgoing, recv_outgoing) = mpsc::channel::<ServerMessage>(4);
        let (send_incoming, recv_incoming) = mpsc::channel::<ClientMessage>(4);

        let session = run(Connection {
            outgoing: send_outgoing,
            incoming: recv_incoming,
        });

        TestSession {
            outgoing: recv_outgoing,
            incoming: send_incoming,
            exit_future: session.boxed(),
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
