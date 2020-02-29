pub mod stdio;

use tokio::sync::mpsc;

use crate::json_rpc::{ClientMessage, ServerMessage};

pub struct Transport {
    /// Channel producing incoming JSON-RPC messages
    pub incoming: mpsc::Receiver<ClientMessage>,

    /// Channel accepting outgoing JSON-RPC messages
    pub outgoing: mpsc::Sender<ServerMessage>,
}
