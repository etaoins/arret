pub mod bytestream;

use tokio::sync::mpsc;

use crate::json_rpc::{ClientMessage, ServerMessage};

pub struct Connection {
    /// Channel producing incoming JSON-RPC messages
    pub incoming: mpsc::Receiver<ClientMessage>,

    /// Channel accepting outgoing JSON-RPC messages
    pub outgoing: mpsc::Sender<ServerMessage>,
}
