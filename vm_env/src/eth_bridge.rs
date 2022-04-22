/// Tx imports and functions.
pub mod tx {
    use anoma::ledger::eth_bridge::storage;
    use anoma::types::transaction::eth_bridge::UpdateQueue;
    use crate::imports::tx;

    pub fn update_queue(data: UpdateQueue) {
        let queue_key = storage::queue_key();
        let newval = data.newval;
        tx::write(queue_key.to_string(), newval);
    }
}
