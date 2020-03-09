use serde_json;
use tokio::io;
use tokio::prelude::*;
use tokio::sync::mpsc;

use crate::json_rpc::{ClientMessage, ServerMessage};
use crate::transport::Connection;

fn parse_header_line(header_line: &str) -> (String, String) {
    let mut parts = header_line.splitn(2, ':');

    let name = parts
        .next()
        .expect("Did not find header name")
        .trim()
        .to_ascii_lowercase();

    let value = parts
        .next()
        .expect("Did not find header value")
        .trim()
        .to_owned();

    (name, value)
}

/// Waits for the passed I/O future and `break`s from the current loop if the pipe is broken
///
/// This is useful to propagate closing `stdout`/`stdin` by closing the respective MPSC channel.
macro_rules! break_on_broken_pipe {
    ($io_future:expr, $message:expr) => {
        if let Err(err) = $io_future.await {
            if err.kind() == io::ErrorKind::BrokenPipe {
                break;
            } else {
                panic!("{}: {:?}", $message, err);
            }
        }
    };
}

pub fn create_connection(
    mut reader: impl io::AsyncBufRead + Unpin + Send + 'static,
    mut writer: impl io::AsyncWrite + Unpin + Send + 'static,
) -> Connection {
    // Allow some concurrency with the session but 4 message is a bit excessive
    // This allows for backpressure on `stdin`/`stdout`
    let (send_outgoing, mut recv_outgoing) = mpsc::channel::<ServerMessage>(4);
    let (mut send_incoming, recv_incoming) = mpsc::channel::<ClientMessage>(4);

    // Write all our responses out sequentially
    tokio::spawn(async move {
        while let Some(response) = recv_outgoing.recv().await {
            let response_bytes =
                serde_json::to_vec(&response).expect("Could not serialise response");

            break_on_broken_pipe!(
                writer.write_all(
                    format!("Content-Length: {}\r\n\r\n", response_bytes.len()).as_bytes()
                ),
                "Could not write response header"
            );

            break_on_broken_pipe!(
                writer.write_all(&response_bytes),
                "Could not write response body"
            );

            break_on_broken_pipe!(writer.flush(), "Could not flush writer");
        }
    });

    tokio::spawn(async move {
        loop {
            let mut content_length: Option<usize> = None;
            let mut line_buffer = String::new();

            // Read the header
            loop {
                break_on_broken_pipe!(
                    reader.read_line(&mut line_buffer),
                    "Could not read header line from stdin"
                );

                if line_buffer == "\r\n" {
                    // Read full header
                    break;
                }

                let (name, value) = parse_header_line(&line_buffer);
                if name == "content-length" {
                    content_length = Some(value.parse().expect("Cannot parse Content-Length"));
                }

                line_buffer.clear();
            }

            let content_length = content_length.expect("Header had no Content-Length");

            // Read the entire content
            let mut read_buffer = Vec::<u8>::new();
            read_buffer.resize(content_length, 0);

            break_on_broken_pipe!(
                reader.read_exact(&mut read_buffer),
                "Could not read body from stdin"
            );

            let client_message: ClientMessage =
                serde_json::from_slice(&read_buffer).expect("Invalid JSON");

            if send_incoming.send(client_message).await.is_err() {
                // Channel closed
                break;
            }
        }
    });

    Connection {
        incoming: recv_incoming,
        outgoing: send_outgoing,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::json_rpc::Notification;

    #[tokio::test]
    async fn test_happy_recv_notification() {
        let body = br#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        let mut message = format!("Content-Length: {}\r\n", body.len()).into_bytes();
        message.extend_from_slice(b"\r\n");
        message.extend_from_slice(body);

        let Connection { mut incoming, .. } = create_connection(
            io::BufReader::new(std::io::Cursor::new(message)),
            Vec::new(),
        );

        let client_message = incoming.recv().await.unwrap();
        assert_eq!(
            ClientMessage::Notification(Notification::new_lsp::<
                lsp_types::notification::Initialized,
            >(lsp_types::InitializedParams {})),
            client_message,
        );
    }
}
