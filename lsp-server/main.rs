mod dispatch;
mod json_rpc;
mod messages;
mod transport;

#[tokio::main]
async fn main() -> Result<(), ()> {
    let transport = transport::stdio::create();
    dispatch::dispatch_messages(transport).await
}
