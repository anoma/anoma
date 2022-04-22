//! A tx sent by the protocol to manage the Ethereum bridge

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    log_string(format!(
        "tx_eth_bridge called with tx_data ({} bytes)",
        tx_data.len()
    ));
    let signed = match SignedTxData::try_from_slice(&tx_data[..]) {
        Ok(signed) => {
            log_string(format!("Got signed data: {:#?}", signed));
            signed
        }
        Err(error) => {
            log_string(format!("Error getting signed data: {:#?}", error));
            panic!()
        }
    };
    let data = match signed.data {
        Some(data) => data,
        None => {
            log_string("No data provided");
            panic!()
        }
    };
    let strct =
        match transaction::eth_bridge::UpdateQueue::try_from_slice(&data[..]) {
            Ok(strct) => {
                log_string(format!("Serialized data to: {:#?}", strct));
                strct
            }
            Err(error) => {
                log_string(format!("Error serializing data: {:#?}", error));
                panic!()
            }
        };
    eth_bridge::update_queue(strct);
}

#[cfg(test)]
mod tests {
    use anoma_tests::tx::*;

    use super::*;

    /// An example test, checking that this transaction performs no storage
    /// modifications.
    #[test]
    fn test_no_op_transaction() {
        // The environment must be initialized first
        tx_host_env::init();

        let tx_data = vec![];
        apply_tx(tx_data);

        let env = tx_host_env::take();
        assert!(env.all_touched_storage_keys().is_empty());
    }
}
