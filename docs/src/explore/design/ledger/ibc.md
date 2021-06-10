# IBC integration

IBC allows the ledgers to interact with each other. IBC is a protocol to agree the consensus state and to send/receive packets between ledgers.

## Update to another ledger
When a ledger executes a transaction including an update to another ledger, it sends a packet and executes the associated validation code like validity predicates to check if the packet has been received. The transaction writes the update to an internal storage subspace called IBC storage. IBC storage as an IBC module makes a packet and send it. After the transaction execution, not only validity predicates but also the IBC validation code associated with the update are executed. If the validation fails, e.g. the destination ledger fails to receive the update due to time out, the transaction is aborted.

TODO The behaviour of the destination ledger

## IBC storage
IBC storage is an IBC module to make a packet and send it to the destination. A transaction stores an update for another ledger to the IBC storage instead of the storage into which other updates are written. The IBC storage doesn't persist the update into the DB, it makes a packet and send it to the destination ledger.

We have the write log to store updates or deletes before commit. A transaction should be able to store an update for another ledger into the write log. The log of the update should be a different modification type from other updates to the same ledger, e.g. `StorageModification::IbcWrite`. At the end of the transaction execution, when the write log has an update for another ledger, IBC storage can check and send it as a packet.

## IBC validation
A validation code validates an update to another ledger has been received on the destination ledger. For the performance, the code should be Rust native code. The validation is executed after all transaction executions. We can execute the validation and validity predicates concurrently.

We can know which update is for another ledger by checking the write log. After all transaction exectuions, we can call validation codes associated with the updates.
