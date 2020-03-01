use serde;
use serde::{Deserialize, Serialize};

// This was originally stolen from rust-analyzer/lsp-server

#[repr(i32)]
pub enum ErrorCode {
    ServerNotInitialized = -32002,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ClientMessage {
    Request(Request),
    Notification(Notification),
}

impl From<Request> for ClientMessage {
    fn from(request: Request) -> ClientMessage {
        ClientMessage::Request(request)
    }
}

impl From<Notification> for ClientMessage {
    fn from(notification: Notification) -> ClientMessage {
        ClientMessage::Notification(notification)
    }
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ServerMessage {
    Response(Response),
    Notification(Notification),
}

impl From<Response> for ServerMessage {
    fn from(response: Response) -> ServerMessage {
        ServerMessage::Response(response)
    }
}

impl From<Notification> for ServerMessage {
    fn from(notification: Notification) -> ServerMessage {
        ServerMessage::Notification(notification)
    }
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct Notification {
    pub method: String,
    pub params: serde_json::Value,
}

impl Notification {
    #[cfg(test)]
    pub fn new(method: impl Into<String>, params: impl Serialize) -> Notification {
        Notification {
            method: method.into(),
            params: serde_json::to_value(params).expect("Could not serialise notification"),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum IdRepr {
    U64(u64),
    String(String),
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(transparent)]
pub struct RequestId(IdRepr);

impl From<u64> for RequestId {
    fn from(id: u64) -> RequestId {
        RequestId(IdRepr::U64(id))
    }
}

impl From<String> for RequestId {
    fn from(id: String) -> RequestId {
        RequestId(IdRepr::String(id))
    }
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct Request {
    pub id: RequestId,
    pub method: String,
    pub params: serde_json::Value,
}

impl Request {
    #[cfg(test)]
    pub fn new(id: RequestId, method: impl Into<String>, params: impl Serialize) -> Request {
        Request {
            id,
            method: method.into(),
            params: serde_json::to_value(params).expect("Could not serialise request"),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct Response {
    pub id: RequestId,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<ResponseError>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct ResponseError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,
}

impl Response {
    pub fn new_ok(id: RequestId, result: impl Serialize) -> Response {
        Response {
            id,
            result: Some(serde_json::to_value(result).expect("Could not serialise result")),
            error: None,
        }
    }

    pub fn new_err(id: RequestId, code: ErrorCode, message: impl Into<String>) -> Response {
        let error = ResponseError {
            code: code as i32,
            message: message.into(),
            data: None,
        };

        Response {
            id,
            result: None,
            error: Some(error),
        }
    }
}