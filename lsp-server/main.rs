mod json_rpc;
mod messages;
mod session;
mod transport;

use tokio::io;

#[tokio::main]
async fn main() -> Result<(), ()> {
    let reader = io::BufReader::new(io::stdin());
    let writer = io::stdout();

    let connection = transport::bytestream::create_connection(reader, writer);
    session::run(connection).await
}
