# IBC integration

[IBC](https://arxiv.org/pdf/2006.15918.pdf) allows a ledger to track another ledger's consensus state using a light client. IBC is a protocol to agree the consensus state and to send/receive packets between ledgers.

## Transaction for IBC
A requester (IBC relayers or users) who wants to execute IBC operations sets IBC packet or message like `MsgCreateAnyClient` as transaction data and submit the following transaction. IBC validity predicate is invoked after this transaction execution by `insert_verifier()` to add an address for IBC `InternalAddress::Ibc`.

The transaction is given an IBC packet or message which specifies what to do as transaction data. Because the packet or message has been encoded and there are some types of messages, it has to check what packet or message is given first and then decodes the message or packet. After the process according to the message, it emits an IBC event.

```rust
#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed =
        key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let messages: Vec<Any> = prost::Message::decode(&signed.data[..]).unwrap();

    let result = match message.type_url.as_str() {
        // decode each message: refer to `ibc-rs`
        create_client::TYPE_URL => {
            let domain_msg = create_client::MsgCreateAnyClient::decode_vec(&any_msg.value)
                .map_err(|e| Kind::MalformedMessageBytes.context(e))?;
            ibc_create_client(domain_msg)
        }
        // other messages
        ...
    };
    match &result {
        Ok(output) => emit_event(output.events),
        Err(e) => {
            tx::log_string(format!("IBC operation faild {}", e));
            unreachable!()
        }
    }
}

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed =
        key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let packet: Packet = prost::Message::decode(&signed.data[..]).unwrap();
    let data: FungibleTokenPacketData = prost::Message::decode(&packet.data[..]).unwrap();
    let result = ibc_transfer(data);
    match &result {
        Ok(output) => emit_event(output.events),
        Err(e) => {
            tx::log_string(format!("IBC transfer faild {}", e));
            unreachable!()
        }
    }
}
```

### Make a packet
IBC-related transaction given some IBC messages or packets would make a packet and send it. For example, when a transaction wants to transfer a token between ledgers, it should make a packet including `FungibleTokenPacketData` to specify the sender, receiver, token, and amount.

### Store a proof and state
These proofs can be stored to the storage on the ledger. It should be prefixed (e.g. `ibc/`) to protect them from other storage operations. The paths(keys) for Tendermint client are defined by [ICS 24](https://github.com/cosmos/ibc/blob/master/spec/core/ics-024-host-requirements/README.md#path-space).

### Emit IBC event
The ledger should set an IBC event to `events` in the ABCI response to allow relayers to get the events. The transaction execution should return `TxResult` including an event. IBC relayer can subscribe the ledger with Tendermint RPC and get the event.

### IBC context
IBC context provides functions to handle IBC modules. IBC-related transaction handles IBC modules through IBC context. [ibc-rs](https://github.com/informalsystems/ibc-rs) defines functions required by these operations. IBC context should implement these functions. For example, `ClientReader` is defined for the read-only part of the client (ICS 2). It has functions for the client module; `client_type()`, `client_state()`, `consensus_state()`, and `client_counter()`.

```rust
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
```

## IBC validity predicate
IBC validity predicate validates that the IBC-related transactions are correct by checking the ledger state. It is executed after a transaction has written IBC-related state. For the performance, IBC validity predicate is a [native validity predicate](ledger/vp.md#native-vps) that are built into the ledger.

```rust
impl NativeVp for IbcVp {
    const ADDR: InternalAddress = InternalAddress::Ibc;

    fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
    where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher
    {
        ...
    }

    fn validate_tx<DB, H>(
        ctx: &mut Ctx<DB, H>,
        tx_data: &[u8],
        keys_changed: &HashSet<Key>,
        verifiers: &HashSet<Address>,
    ) -> bool
    where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher
    {
        // check the signature for sending token (same as vp_user)

        // validate the escrowed/unescrowed amount
        // for all tokens that were updated by IBC-related transactions
    }
}
```

## Relayer (ICS 18)
IBC relayer monitors the ledger, gets the status, state and proofs on the ledger, and requests transactions to the ledger via Tendermint RPC according to IBC protocol. For relayers, the ledger has to make a packet, emits an IBC event and stores proofs if needed. And, a relayer has to support Anoma ledger to query and validate the ledger state. It means that `Chain` in IBC Relayer of [ibc-rs](https://github.com/informalsystems/ibc-rs) should be implemented for Anoma.

```rust
impl Chain for Anoma {
    ...
}
```

## Transfer (ICS 20)
![transfer](./ibc/transfer.svg  "transfer")
