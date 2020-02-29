use serde;
use serde::{Deserialize, Serialize};

// This was originally stolen from rust-analyzer/lsp-server

#[repr(i32)]
pub enum ErrorCode {
    MethodNotFound = -32601,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum IncomingMessage {
    Request(Request),
    Notification(Notification),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Notification {
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum IdRepr {
    U64(u64),
    String(String),
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(transparent)]
pub struct RequestId(IdRepr);

#[derive(Serialize, Deserialize)]
pub struct Request {
    pub id: RequestId,
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Response {
    pub id: RequestId,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<ResponseError>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ResponseError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,
}

impl Response {
    pub fn new_ok<R: Serialize>(id: RequestId, result: R) -> Response {
        Response {
            id,
            result: Some(serde_json::to_value(result).expect("Could not serialise result")),
            error: None,
        }
    }

    pub fn new_err(id: RequestId, code: ErrorCode, message: String) -> Response {
        let error = ResponseError {
            code: code as i32,
            message,
            data: None,
        };

        Response {
            id,
            result: None,
            error: Some(error),
        }
    }
}
