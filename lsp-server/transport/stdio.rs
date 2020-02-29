use serde_json;
use tokio::io;
use tokio::prelude::*;
use tokio::sync::mpsc;

use crate::dispatch;
use crate::json_rpc;

fn parse_header_component(component: Vec<u8>) -> String {
    String::from_utf8(component)
        .expect("Non-UTF8 header encoding")
        .trim()
        .to_string()
}

fn parse_header_line(header_line: &[u8]) -> (String, String) {
    let mut parts = header_line.splitn(2, |c| *c == b':');

    let name = parse_header_component(parts.next().expect("Did not find header name").to_owned())
        .to_ascii_lowercase();

    let value = parse_header_component(parts.next().expect("Did not find header value").to_owned());

    (name, value)
}

pub async fn main_loop() {
    let mut reader = io::BufReader::new(io::stdin());

    let (send_response, mut recv_response) = mpsc::channel::<json_rpc::Response>(4);

    // Write all our responses out sequentially
    tokio::spawn(async move {
        let mut writer = io::stdout();

        loop {
            let response = recv_response.recv().await;

            let response_bytes =
                serde_json::to_vec(&response).expect("Could not serialise response");

            writer
                .write_all(format!("Content-Length: {}\r\n\r\n", response_bytes.len()).as_bytes())
                .await
                .expect("Could not write response header");

            writer
                .write_all(&response_bytes)
                .await
                .expect("Could not write response body");

            writer.flush().await.expect("Could not flush writer");
        }
    });

    loop {
        let mut content_length: Option<usize> = None;
        let mut read_buffer = Vec::<u8>::new();

        // Read the header
        while let Ok(_) = reader.read_until(b'\n', &mut read_buffer).await {
            if read_buffer == b"\r\n" {
                // Read full header
                break;
            }

            let (name, value) = parse_header_line(&read_buffer);
            if name == "content-length" {
                content_length = Some(value.parse().expect("Cannot parse Content-Length"));
            }

            read_buffer.clear();
        }

        let content_length = content_length.expect("Header had no Content-Length");

        // Read the entire content
        read_buffer.resize(content_length, 0);
        reader
            .read_exact(&mut read_buffer)
            .await
            .expect("Could not read body");

        let incoming_message: json_rpc::IncomingMessage =
            serde_json::from_slice(&read_buffer).expect("Invalid JSON");

        // Asynchronously dispatch while we consume the next incoming message
        dispatch::dispatch_incoming_message(incoming_message, send_response.clone());
    }
}
