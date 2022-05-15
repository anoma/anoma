use anoma_tx_prelude::{
    log_string, transaction, write, BorshDeserialize, Signed,
};

const TX_NAME: &str = "tx_enqueue_eth_transfer";

const ETH_BRIDGE_QUEUE_STORAGE_KEY: &str = "#atest1v9hx7w36g42ysgzzwf5kgem9ypqkgerjv4ehxgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpq8f99ew/queue";

fn log(msg: &str) {
    log_string(format!("[{}] {}", TX_NAME, msg))
}

fn fatal(msg: &str, err: impl std::error::Error) -> ! {
    log(&format!("ERROR: {} - {:?}", msg, err));
    panic!()
}

fn fatal_msg(msg: &str) -> ! {
    log(msg);
    panic!()
}

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    log(&format!("called with tx_data - {} bytes", tx_data.len()));
    let signed: Signed<Vec<u8>> = match Signed::try_from_slice(&tx_data) {
        Ok(signed) => {
            log("deserialized Signed<Vec<u8>>");
            signed
        }
        Err(error) => fatal("deserializing Signed<Vec<u8>>", error),
    };
    if signed.data.is_empty() {
        fatal_msg("data is empty")
    }
    log(&format!("got data - {} bytes", signed.data.len()));
    // we don't verify the signature here - the VP should do that

    log("attempting to write data to the Ethereum bridge queue");
    write(ETH_BRIDGE_QUEUE_STORAGE_KEY, &signed.data);
}
