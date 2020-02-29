use crate::json_rpc::{ErrorCode, IncomingMessage, Request, Response};
use crate::messages;
use crate::transport::Transport;
use lsp_types;
use serde_json;

fn handle_non_lifetime_request(request: Request) -> Response {
    Response::new_err(
        request.id,
        ErrorCode::MethodNotFound,
        "Method not found".to_owned(),
    )
}

/// Runs a dispatch loop against the provided transport
///
/// On a clean exit (`shutdown` followed by `exit`) this will return `Ok`, otherwise it will return
/// `Err`.
pub async fn dispatch_messages(transport: Transport) -> Result<(), ()> {
    let Transport {
        mut incoming,
        mut outgoing,
    } = transport;

    /// Sends the outgoing message or returns `Err` if the send channel is closed
    ///
    /// This will cause us to exit uncleanly if our transport closes unexpectedly.
    macro_rules! send_or_return_err {
        ($outgoing_message:expr) => {
            if outgoing.send($outgoing_message.into()).await.is_err() {
                eprintln!("Transport unexpectedly closed");
                return Err(());
            }
        };
    }

    // Wait for initialize
    while let Some(incoming_message) = incoming.recv().await {
        match incoming_message {
            IncomingMessage::Notification(notification) => {
                if notification.method == "exit" {
                    // Unclean exit
                    return Err(());
                }
            }
            IncomingMessage::Request(request) if request.method.as_str() == "initialize" => {
                let initialize_params: lsp_types::InitializeParams =
                    serde_json::from_value(request.params)
                        .expect("Could not parse initialize params");

                let response = messages::initialize::handle(initialize_params);
                send_or_return_err!(Response::new_ok(request.id, response));

                break;
            }
            IncomingMessage::Request(request) => {
                send_or_return_err!(Response::new_err(
                    request.id,
                    ErrorCode::ServerNotInitialized,
                    "Server not initialized".to_owned(),
                ));
            }
        }
    }

    // Process normal messages until we receive a shutdown request
    while let Some(incoming_message) = incoming.recv().await {
        match incoming_message {
            IncomingMessage::Notification(notification) => {
                if notification.method == "exit" {
                    // Unclean exit
                    return Err(());
                }
            }
            IncomingMessage::Request(request) if request.method == "shutdown" => {
                send_or_return_err!(Response::new_ok(request.id, serde_json::Value::Null));
            }
            IncomingMessage::Request(request) => {
                send_or_return_err!(handle_non_lifetime_request(request));
                break;
            }
        }
    }

    // Wait for exit
    while let Some(incoming_message) = incoming.recv().await {
        match incoming_message {
            IncomingMessage::Notification(notification) => {
                if notification.method == "exit" {
                    return Ok(());
                }
            }
            IncomingMessage::Request(request) => {
                send_or_return_err!(Response::new_err(
                    request.id,
                    ErrorCode::InvalidRequest,
                    "Shutting down".to_owned(),
                ));
            }
        }
    }

    // Receiver channel unexpectedly closed
    Err(())
}
