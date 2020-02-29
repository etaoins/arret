mod dispatch;
mod json_rpc;
mod messages;
mod transport;

#[tokio::main]
async fn main() {
    transport::stdio::main_loop().await
}
