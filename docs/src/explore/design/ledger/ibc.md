# IBC integration

[IBC](https://arxiv.org/pdf/2006.15918.pdf) allows a ledger to track another ledger's consensus state using a light client. IBC is a protocol to agree the consensus state and to send/receive packets between ledgers.

## Transaction for IBC
A requester (IBC relayer or user) who wants to execute IBC operations on a ledger sets IBC packet or message like `MsgCreateAnyClient` as transaction data and submit the following transaction. IBC validity predicate is invoked after this transaction execution to check the IBC operation is validated. The trigger to invoke IBC validity predicate is changing IBC-related keys (e.g. prefixed with `ibc/`).

The transaction is given an IBC packet or message which specifies what to do. Because the packet or message has been encoded and stored in transaction data and there are some types of messages, it has to check what packet or message is given first and then decodes the message or packet. Then, it can modify the ledger state by writing not only data specified in the transaction but also IBC-related data on the storage sub-space. After the process according to the packet or message, it emits an IBC event.

- Transaction given a message
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
  ```

- Transaction given a packet
  ```rust
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
IBC-related transaction for some messages or packets makes a packet. For example, when a transaction wants to transfer a token between ledgers, it should make a packet including `FungibleTokenPacketData` to specify the sender, receiver, token, and amount.

### Store IBC-related data
The IBC-related transaction can write IBC-related data to check the state or to be proved by other ledgers according to IBC protocol. Its storage key should be prefixed (e.g. `ibc/`) to protect them from other storage operations. The paths(keys) for Tendermint client are defined by [ICS 24](https://github.com/cosmos/ibc/blob/master/spec/core/ics-024-host-requirements/README.md#path-space).

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
IBC validity predicate validates that the IBC-related transactions are correct by checking the ledger state including prior and posterior. It is executed after a transaction has written IBC-related state. For the performance, IBC validity predicate is a [native validity predicate](ledger/vp.md#native-vps) that are built into the ledger.

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
      ...
    }
}
```

IBC validity predicate has to execute the following validations for state changes of IBC modules.

### Client
- CreateClient (`clients/{identifier}` is inserted)
  - Check the consistency about the client type by reading them from `clients/{identifier}/clientType`, `clients/{identifier}/clientState` and `clients/{identifier}/consensusStates/{height}`.

- UpdateClient (`clients/{identifier}/consensusStates/{height}` is inserted)
  - Verify the new header with the stored client state’s validity predicate and consensus state

- UpgradeClient
  - TODO

### Connection
- ConnectionOpenInit (`connections/{identifier}` is inserted and the state is `INIT`)
  - Check that the client identifier is valid
    - Check that `clients/{client-id}/clientState` exists on this ledger

- ConnectionOpenTry (`connections/{identifier}` is inserted and the state is `TRYOPEN`)
  - Check that the client identifier is valid
    - Check that `clients/{identifier}/clientState` exists on this ledger
  - Check that the version is compatible
  - Check that the client of the counterpart ledger exists
  - Verify the proof that the counterpart ledger has stored the identifier
    - Check that `clients/{identifier}/clientState` exists on the counterpart ledger with the proof
  - Verify the proof that the counterpart ledger's client is using to validate this ledger has the correct consensus state
    - Check that `clients/{identifier}/consensusStates/{height}` exists on the counterpart ledger with the proof

- ConnectionOpenAck (the state of `connections/{identifier}` is updated from `INIT` to `OPEN`)
  - Same as ConnectionOpenTry

- ConnectionOpenConfirm (the state of `connections/{identifier}` is updated from `TRYOPEN` to `OPEN`)
  - Verify that the counterparty ledger has marked `OPEN` with the proof
    - Check that the state of `connections/{identifier}` on the counterpart ledger is `OPEN` with the proof

### Channel
- ChanOpenInit (`channelEnds/ports/{port-id}/channels/{channel-id}` is inserted and the state is `INIT`)
  - Nothing to do

- ChanOpenTry (`channelEnds/ports/{port-id}/channels/{channel-id}` is inserted and the state is `TRYOPEN`)
  - Verify the proof that the counterpart ledger has stored the port identifier and the channel identifier
    - Check that `channelEnds/ports/{port-id}/channels/{channel-id}` exists on the counterpart ledger with the proof
  - Check that the port is owned
  - Check that the version is compatible

- ChanOpenAck (the state of `channelEnds/ports/{port-id}/channels/{channel-id}` is updated from `INIT` to `OPEN`)
  - Verify the proof that the counterpart ledger has stored the port identifier and the channel identifier
    - Check that `channelEnds/ports/{port-id}/channels/{channel-id}` exists on the counterpart ledger with the proof

- ChanOpenConfirm (the state of `channelEnds/ports/{port-id}/channels/{channel-id}` is updated from `TRYOPEN` to `OPEN`)
  - Verify that the counterparty ledger has marked `OPEN` with the proof
    - Check that the state of `channelEnds/ports/{port-id}/channels/{channel-id}` on the counterpart ledger is `OPEN` with the proof

- SendPacket (`nextSequenceSend/ports/{port-id}/channels/{channel-id}` is updated)
  - Check that the connection and the channel are open
  - Check that the port is owned
  - Check that the packet metadata matches the channel and connection information
  - Checks that the timeout height specified has not already passed on the destination ledger
    - Check that `clients/{identifier}/clientState` and `clients/{identifier}/consensusStates/{height}`

- RecvPacket (`nextSequenceRecv/ports/{port-id}/channels/{channel-id}` is updated)
  - Check that the connection and the channel are open
  - Check that the port is owned
  - Check that the packet metadata matches the channel and connection information
  - Check that the packet sequence is the next sequence the channel end expects to receive
  - Checks that the timeout height has not yet passed
    - Check that `clients/{identifier}/clientState` and `clients/{identifier}/consensusStates/{height}`
  - Verify the proof that the counterpart ledger has stored the commitment
    - Check that `commitments/ports/{identifier}/channels/{identifier}/packets/{sequence}` exists on the counterpart ledger with the proof

- AcknowledgePacket (`nextSequenceAck/ports/{identifier}/channels/{identifier}` is updated and `commitments/ports/{identifier}/channels/{identifier}/packets/{sequence}` is deleted)
  - Check that the connection and the channel are open
  - Check that the port is owned
  - Check that the packet metadata matches the channel and connection information
  - Check that the packet was actually sent on this channel
  - Check that the packet sequence is the next sequence the channel end expects to acknowledge
  - Verify the proof that the counterpart ledger has stored the acknowledgement data
    - Check that `acks/ports/{identifier}/channels/{identifier}/acknowledgements/{sequence}` exists on the counterpart ledger with the proof

- TimeoutPacket (`commitments/ports/{identifier}/channels/{identifier}/packets/{sequence}` is deleted)
  - TODO

- TimeoutOnClose
  - TODO

## Relayer (ICS 18)
IBC relayer monitors the ledger, gets the status, state and proofs on the ledger, and requests transactions to the ledger via Tendermint RPC according to IBC protocol. For relayers, the ledger has to make a packet, emits an IBC event and stores proofs if needed. And, a relayer has to support Anoma ledger to query and validate the ledger state. It means that `Chain` in IBC Relayer of [ibc-rs](https://github.com/informalsystems/ibc-rs) should be implemented for Anoma like [that of CosmosSDK](https://github.com/informalsystems/ibc-rs/blob/master/relayer/src/chain/cosmos.rs).

```rust
impl Chain for Anoma {
    ...
}
```

## Transfer (ICS 20)
![transfer](./ibc/transfer.svg  "transfer")
