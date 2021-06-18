# IBC integration

[IBC](https://arxiv.org/pdf/2006.15918.pdf) allows the ledgers to interact with each other. IBC is a protocol to agree the consensus state and to send/receive packets between ledgers.

## IBC validity predicate
IBC validity predicate verifies the ledger state and the IBC modules by interacting with the IBC handler, and updates values according to IBC message. Also, it makes a packet, store proofs which can be proven later, and emit an IBC event to be scanned by a relayer.

IBC validity predicate is executed after a transaction has added an IBC message. So, it is executed after all transaction executions. It is given IBC message which specifies what to do. Because the message has been encoded and there are some types of messages, it has to check what message is given first and then decodes the message. After the process according to the message, it emits an IBC event.

For the performance, the code should be Rust native code unlike usual validity predicates are WASM. 

```rust
pub struct IbcVp {
    ctx: IbcContext,
}

impl IbcVp {
    pub fn run(&self, message: Any) -> IbcVpResult {
        let result = match message.type_url.as_str() {
            // decode each message: refer to `ibc-rs`
            create_client::TYPE_URL => {
                let domain_msg = create_client::MsgCreateAnyClient::decode_vec(&any_msg.value)
                    .map_err(|e| Kind::MalformedMessageBytes.context(e))?;
                ...
            }
            // other messages
            ...
        }
        emitEvent(result.events);
        result
    }
}

pub struct IbcContext {...}

// ICS 2
impl ClientReader for IbcContext {...}
impl ClientKeeper for IbcContext {...}
// ICS 3
impl ConnectionReader for IbcContext {...}
impl ConnectionKeeper for IbcContext {...}
// ICS 4
impl ChannelReader for IbcContext {...}
impl ChannelKeeper for IbcContext {...}
// ICS 5
impl PortReader for IbcContext {...}
// ICS 18 (for relayer)
impl Ics18Context for IbcContext {...}
```

### Make a packet
IBC validity predicate given some messages would make a packet and send it. For example, when IBC validity predicate is given `MsgTransfer` for transfer between ledgers, it should make a packet including `FungibleTokenPacketData` to specify the sender, receiver, token, amount.

### Store a proof and state
These proofs can be stored to the storage on the ledger. It should be prefixed (e.g. `ibc/`) to protect them from other storage operations. The paths(keys) for Tendermint client are defined by [ICS 24](https://github.com/cosmos/ibc/blob/master/spec/core/ics-024-host-requirements/README.md#path-space).

### Emit IBC event
Relayer can subscribe the ledger with Tendermint RPC. The ledger should set IBC events to `events` in the response to allow for relayers to get the events.

## Transaction for IBC
A requester (relayers or users) who wants to execute IBC operations sets IBC messages like `MsgCreateAnyClient` as transaction data and submit the following transaction. A function e.g. `ibc_message()` to add the messages should be provided.

```rust
#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed =
        key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let messages: Vec<Any> = prost::Message::decode(&signed.data[..]).unwrap();
    ibc_message(messages);
}
```

For transfer between ledgers in a user transaction, a function e.g. `ibc_transfer()` to make an IBC message (MsgTransfer) is provided.

```rust
#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed =
        key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let transfer = token::Transfer::try_from_slice(&signed.data[..]).unwrap();
    let token::Transfer {
        source,
        target,
        token,
        amount,
    } = transfer;
    ibc_transfer(transfer);
}
```

### IBC message
When a transaction has IBC messages, the ledger should call IBC validity predicate. A transaction stores the messages to an IBC message list instead of the storage into which other updates are written. These messages are not persisted into the DB on the ledger. After the all transactions are executed, the ledger checks the list and executes IBC validity predicate for each message.

We have the write log to store updates or deletes before commit. A transaction should be able to store messages into the write log. The log of a message should be a different modification type from other updates to the same ledger, e.g. `StorageModification::IbcMessage`. At the end of the transaction execution, when the write log has these messages, the ledger executes IBC validity predicate.

- Messages for Client (ICS 2)
  - MsgCreateClient
  - MsgUpdateClient
  - MsgUpgradeClient

- Messages for Connection (ICS 3)
  - MsgConnectionOpenInit
  - MsgConnectionOpenTry
  - MsgConnectionOpenAck
  - MsgConnectionOpenConfirm

- Messages for Channel (ICS 4)
  - MsgChannelOpenInit
  - MsgChannelOpenTry
  - MsgChannelOpenAck
  - MsgChannelOpenConfirm
  - MsgChannelCloseInit
  - MsgChannelCloseConfirm
  - MsgRecvPacket
  - MsgAcknowledgement
  - MsgTimeout
  - MsgTimeoutOnClose

- Message for Transfer (ICS 20)
  - MsgTransfer

## Relayer (ICS 18)
A relayer monitors the ledger, gets the status, state and proofs on the ledger, and requests transactions to the ledger via Tendermint RPC according to IBC protocol. For relayers, the ledger has to make a packet, emits an IBC event and stores proofs if needed. And, a relayer has to support Anoma ledger to query and validate the ledger state. It means that `Chain` in Relayer of `ibc-rs` should be implemented for Anoma.

```rust
impl Chain for Anoma {
    ...
}
```

## Transfer (ICS 20)
![transfer](./ibc/transfer.svg  "transfer")
