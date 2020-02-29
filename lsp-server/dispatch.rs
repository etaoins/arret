use lsp_types;
use serde_json;
use tokio;
use tokio::sync::mpsc;

use crate::json_rpc;
use crate::messages;

async fn handle_request(request: json_rpc::Request) -> json_rpc::Response {
    match request.method.as_str() {
        "initialize" => {
            let initialize_params: lsp_types::InitializeParams =
                serde_json::from_value(request.params).expect("Could not parse initialize params");

            let response = messages::initialize::handle(initialize_params);
            json_rpc::Response::new_ok(request.id, response)
        }
        _ => json_rpc::Response::new_err(
            request.id,
            json_rpc::ErrorCode::MethodNotFound,
            "Method not found".to_owned(),
        ),
    }
}

pub fn dispatch_incoming_message(
    incoming_message: json_rpc::IncomingMessage,
    mut send_response: mpsc::Sender<json_rpc::Response>,
) {
    match incoming_message {
        json_rpc::IncomingMessage::Notification(_) => {}
        json_rpc::IncomingMessage::Request(request) => {
            // Run this asynchronously with everything else
            tokio::spawn(async move {
                let response = handle_request(request).await;

                send_response
                    .send(response)
                    .await
                    .expect("Response channel is closed");
            });
        }
    }
}
