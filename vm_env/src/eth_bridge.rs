/// Tx imports and functions.
pub mod tx {
    use anoma::ledger::eth_bridge::storage;
    use anoma::ledger::eth_bridge::storage::queue_key;
    use anoma::types::transaction::eth_bridge::{
        TransferFromEthereum, UpdateQueue,
    };

    use crate::imports::tx;
    use crate::tx_prelude::log_string;

    pub fn update_queue(data: UpdateQueue) {
        log_string(format!(
            "update_queue tx being executed ({} messages to enqueue)",
            data.enqueue.len()
        ));
        let queue_key = storage::queue_key();
        let mut queue: Vec<TransferFromEthereum> =
            if tx::has_key(queue_key.to_string()) {
                log_string("queue key exists");
                tx::read(queue_key.to_string()).unwrap()
            } else {
                log_string("initializing queue for the first time");
                tx::write(
                    queue_key.to_string(),
                    Vec::<TransferFromEthereum>::new(),
                );
                vec![]
            };
        log_string(format!("got existing queue: {:#?}", queue));
        for transfer in data.enqueue {
            queue.push(transfer);
        }
        tx::write(queue_key.to_string(), queue);
    }
}
