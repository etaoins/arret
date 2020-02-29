pub mod stdio;

use tokio::sync::mpsc;

use crate::json_rpc::{IncomingMessage, OutgoingMessage};

pub struct Transport {
    /// Channel producing incoming JSON-RPC messages
    pub incoming: mpsc::Receiver<IncomingMessage>,

    /// Channel accepting outgoing JSON-RPC messages
    pub outgoing: mpsc::Sender<OutgoingMessage>,
}
