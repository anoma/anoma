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
              create_client(domain_msg.client_state, &domain_msg.consensus_state)
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
The IBC-related transaction can write IBC-related data to check the state or to be proved by other ledgers according to IBC protocol. Its storage key should be prefixed with `InternalAddress::Ibc` and a specific character `^` to protect them from other storage operations. The paths(keys) for Tendermint client are defined by [ICS 24](https://github.com/cosmos/ibc/blob/master/spec/core/ics-024-host-requirements/README.md#path-space). For example, a client state will be stored with a key `#IBC/^/clients/{client_id}/clientState`.

### Emit IBC event
The ledger should set an IBC event to `events` in the ABCI response to allow relayers to get the events. The transaction execution should return `TxResult` including an event. IBC relayer can subscribe the ledger with Tendermint RPC and get the event.

### Handle IBC modules
IBC-related transactions should call functions to handle IBC modules. These functions are defined in [ibc-rs](https://github.com/informalsystems/ibc-rs) in traits (e.g. [`ClientReader`](https://github.com/informalsystems/ibc-rs/blob/d41e7253b997024e9f5852735450e1049176ed3a/modules/src/ics02_client/context.rs#L14)). But we can implement IBC-related operations (e.g. `create_client()`) without these traits because Anoma WASM transaction accesses the storage through the host environment functions.

```rust
/* shared/src/types/storage.rs */

impl Key {
    ...

    /// Check if the given key is a key to IBC-related data
    pub fn is_ibc_key() -> bool {
        // check if the key has the reserved prefix
    }

    /// Returns a key of the IBC-related data
    /// Only this function can push the reserved prefix
    pub fn ibc_key(path: impl AsRef<str>) -> Result<Self> {
        // make a Key for IBC-related data with the reserved prefix
    }
}
```

```rust
/* vm_env/src/ibc.rs */

pub fn create_client(client_state: &ClientState, consensus_state: &AnyConsensusState) -> HandlerResult<ClientResult> {
    use crate::imports::tx;

    ...
    let key = Key::ibc_key(client_counter_key);
    let id_counter = tx::read(key).unwrap_or_default();
    let client_id = ClientId::new(client_state.client_type(), id_counter).expect("cannot get an IBC client ID");
    tx::write(key, id_counter + 1);

    let key = Key::ibc_key(client_type_key);
    tx::write(key, client_state.client_type());
    let key = Key::ibc_key(client_state_key);
    tx::write(key, client_state);
    let key = Key::ibc_key(consensus_state_key);
    tx::write(key, consensus_state);

    // make a result
    ...
}
```

## IBC validity predicate
IBC validity predicate validates that the IBC-related transactions are correct by checking the ledger state including prior and posterior. It is executed after a transaction has written IBC-related state. If the result is true, the IBC-related mutations are committed and the events are returned. If the result is false, the IBC-related mustations are dropped and the events aren't emitted. For the performance, IBC validity predicate is a [native validity predicate](ledger/vp.md#native-vps) that are built into the ledger.

IBC validity predicate has to execute the following validations for state changes of IBC modules.

```rust
/* shared/src/ledger/ibc.rs */

pub struct Ibc<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

impl NativeVp for Ibc {
    const ADDR: InternalAddress = InternalAddress::Ibc;

    fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
    where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher
    {
        // initialize the counters of client, connection and channel module
    }

    fn validate_tx(
        tx_data: &[u8],
        data_from_tx: &HashMap<Key, Vec<u8>>,
        keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        for key in keys_changed {
            if !key.is_ibc_key() {
                continue;
            }

            match get_ibc_prefix(key) {
                // client
                "clients" => {
                    let client_id = get_client_id(key);
                    // Check the client state change
                    //   - created or updated
                    match check_client_state(client_id) {
                        StateChange::Created => {
                            // "CreateClient"
                            // Assert that the corresponding consensus state exists
                        }
                        StateChange::Update => {
                            match get_header(data_from_tx) {
                                Some(header) => {
                                    // "UpdateClient"
                                    // Verify the header with the stored client state’s validity predicate and consensus state
                                    //   - Refer to `ibc-rs::ics02_client::client_def::check_header_and_update_state()`
                                }
                                None => {
                                    // "UpgradeClient"
                                    // Verify the proofs to check the client state and consensus state
                                    //   - Refer to `ibc-rs::ics02_client::client_def::verify_upgrade_and_update_state()`
                                }
                            }
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                // connection handshake
                "connections" => {
                    let connection_id = get_connection_id(key);
                    // Check the connection state change
                    //   - none => INIT, none => TRYOPEN, INIT => OPEN, or TRYOPEN => OPEN
                    match check_connection_state(connection_id) {
                        StateChange::Created => {
                            // "ConnectionOpenInit"
                            // Assert that the corresponding client exists
                        }
                        StateChange::Updated => {
                            // Assert that the version is compatible

                            // Verify the proofs to check the counterpart ledger's state is expected
                            //   - The state can be inferred from the own connection state change
                            //   - Use `ibc-rs::ics03_connection::handler::verify::verify_proofs()`
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                // channel handshake or closing
                "channelEnds" => {
                    // Assert that the port is owend

                    // Assert that the corresponding connection exists

                    // Assert that the version is compatible

                    // Check the channel state change
                    //   - none => INIT, none => TRYOPEN, INIT => OPEN, TRYOPEN => OPEN, or OPEN => CLOSED
                    match check_channel_state(channel_id) {
                        StateChange::Created => {
                            // "ChanOpenInit"
                            continue;
                        }
                        StateChange::Closed => {
                            // OPEN => CLOSED
                            match get_proofs(data_from_tx) {
                                Some(proofs) => {
                                    // "ChanCloseConfirm"
                                    // Verify the proofs to check the counterpart ledger's channel has been closed
                                    //   - Use `ibc-rs::ics04_connection::handler::verify::verify_channel_proofs()`
                                }
                                None => {
                                    // "ChanCloseInit"
                                    continue;
                                }
                            }
                        }
                        StateChange::Updated => {
                            // Verify the proof to check the counterpart ledger's state is expected
                            //   - The state can be inferred from the own channel state change
                            //   - Use `ibc-rs::ics04_connection::handler::verify::verify_channel_proofs()`
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                "nextSequenceSend" => {
                    // "SendPacket"
                    let packet = get_packet(key, data_from_tx)?;

                    // Assert that the packet metadata matches the channel and connection information
                    //   - the port is owend
                    //   - the channel exists
                    //   - the counterparty information is valid
                    //   - the connection exists

                    // Assert that the connection and channel are open

                    // Assert that the packet sequence is the next sequence that the channel expects

                    // Assert that the timeout height and timestamp have not passed on the destination ledger

                    // Assert that the commitment has been stored
                }

                "nextSequenceRecv" => {
                    // "RecvPacket"
                    let packet = get_packet(key, data_from_tx)?;
                    let proofs = get_proofs(key, data_from_tx)?;

                    // Assert that the packet metadata matches the channel and connection information

                    // Assert that the connection and channel are open

                    // Assert that the packet sequence is the next sequence that the channel expects (Ordered channel)

                    // Assert that the timeout height and timestamp have not passed on the destination ledger

                    // Assert that the receipt and acknowledgement have been stored

                    // Verify the proofs that the counterpart ledger has stored the commitment
                    //   - Use `ibc-rs::ics04_connection::handler::verify::verify_packet_recv_proofs()`
                }

                "nextSequenceAck" => {
                    // "Acknowledgement"
                    let packet = get_packet(key, data_from_tx)?;
                    let proofs = get_proofs(key, data_from_tx)?;

                    // Assert that the packet metadata matches the channel and connection information

                    // Assert that the connection and channel are open

                    // Assert that the packet sequence is the next sequence that the channel expects (Ordered channel)

                    // Assert that the commitment has been deleted

                    // Verify that the packet was actually sent on this channel
                    //   - Get the stored commitment and compare it with a commitment made from the packet

                    // Verify the proofs to check the acknowledgement has been written on the counterpart ledger
                    //   - Use `ibc-rs::ics04_connection::handler::verify::verify_packet_acknowledgement_proofs()`
                }

                "commitments" => {
                    let packet = get_packet(key, data_from_tx)?;
                    // check if the commitment is deleted
                    match check_commitment_state(key) {
                        StateChange::Deleted => {
                            // Assert that the packet was actually sent on this channel
                            //   - Get the stored commitment and compare it with a commitment made from the packet

                            // Check the channel state change
                            match check_channel_state(channel_id) {
                                StateChange::Closed => {
                                    // "Timeout"
                                    // Assert that the connection and channel are open

                                    // Assert that the counterpart ledger has exceeded the timeout height or timestamp

                                    // Assert that the packet sequence is the next sequence that the channel expects (Ordered channel)
                                }
                                _ => {
                                    // "TimeoutOnClose"
                                    // Assert that the packet sequence is the next sequence that the channel expects (Ordered channel)

                                    // Verify the proofs to check the counterpart ledger's state is expected
                                    //   - The channel state on the counterpart ledger should be CLOSED
                                    //   - Use `ibc-rs::ics04_connection::handler::verify::verify_channel_proofs()`
                                }
                                // Verify the proofs to check the packet has not been confirmed on the counterpart ledger
                                //   - For ordering channels, use `ibc-rs::ics04_connection::handler::verify::verify_next_sequence_recv()`
                                //   - For not-ordering channels, use `ibc-rs::ics04_connection::handler::verify::verify_packet_receipt_absence()`
                            }
                        }
                        StateChange::Created | StateChange::Updated => {
                            // They are checked in other cases
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                "ports" | "receipts" | "acks" => {
                    // They are checked in other cases
                }

                _ => {
                    // unknown prefix
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }
}
```

### Handle IBC modules
Like IBC-related transactions, the validity predicate should handle IBC modules. It only reads the prior or the posterior state to validate them. `Keeper` to write IBC-related data aren't required, but we needs to implement `Reader` for both the prior and the posterior state. To use verification functions in `ibc-rs`, implementations for traits for IBC modules (e.g. `ClientReader`) should be for the posterior state. For example, we can call [`verify_proofs()`](https://github.com/informalsystems/ibc-rs/blob/d41e7253b997024e9f5852735450e1049176ed3a/modules/src/ics03_connection/handler/verify.rs#L14) with the IBC's context in a step of the connection handshake: `verify_proofs(ibc, client_state, &conn_end, &expected_conn, proofs)`.

```rust
/* shared/src/ledger/ibc.rs */

pub struct Ibc<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

// Add implementations to get the posterior state for validations in `ibc-rs`
// ICS 2
impl<'a, DB, H> ClientReader for Ibc<'a, DB, H> {...}
// ICS 3
impl<'a, DB, H> ConnectionReader for Ibc<'a, DB, H> {...}
// ICS 4
impl<'a, DB, H> ChannelReader for Ibc<'a, DB, H> {...}
// ICS 5
impl<'a, DB, H> PortReader for Ibc<'a, DB, H> {...}

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    ...

    // Add functions to get the prior state if needed
    pub fn client_type_pre(&self, client_id: &ClientId) -> Result<Option<ClientType>> {
        ...
    }
    pub fn client_state_pre(&self, client_id: &ClientId) -> Result<Option<AnyClientState>> {
        ...
    }
    pub fn consensus_state_pre(&self, client_id: &ClientId, height: Height) -> Result<Option<AnyConsensusState>> {
        ...
    }
    pub fn client_counter_pre(&self) -> Result<u64> {
        ...
    }
    ...
}
```

## Relayer (ICS 18)
IBC relayer monitors the ledger, gets the status, state and proofs on the ledger, and requests transactions to the ledger via Tendermint RPC according to IBC protocol. For relayers, the ledger has to make a packet, emits an IBC event and stores proofs if needed. And, a relayer has to support Anoma ledger to query and validate the ledger state. It means that `Chain` in IBC Relayer of [ibc-rs](https://github.com/informalsystems/ibc-rs) should be implemented for Anoma like [that of CosmosSDK](https://github.com/informalsystems/ibc-rs/blob/master/relayer/src/chain/cosmos.rs).

```rust
impl Chain for Anoma {
    ...
}
```

## Transfer (ICS 20)
![transfer](./ibc/transfer.svg  "transfer")
