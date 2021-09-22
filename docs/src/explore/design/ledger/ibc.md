# IBC integration

[IBC](https://arxiv.org/pdf/2006.15918.pdf) allows a ledger to track another ledger's consensus state using a light client. IBC is a protocol to agree the consensus state and to send/receive packets between ledgers.

## Transaction for IBC
A requester (IBC relayer or user) who wants to execute IBC operations on a ledger sets required data (packet, proofs, module state, timeout height/timestamp etc.) as transaction data, and submit a transaction with the transaction data. The transaction executes the specified IBC operation. IBC validity predicate is invoked after this transaction execution to verify the IBC operation. The trigger to invoke IBC validity predicate is changing IBC-related keys prefixed with `#encoded-ibc-address/`.

The transaction can modify the ledger state by writing not only data specified in the transaction but also IBC-related data on the storage sub-space. Also, it emits an IBC event at the end of the transaction.

- Transaction to create a client
  ```rust
  #[transaction]
  fn apply_tx(tx_data: Vec<u8>) {
      let signed =
          key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
      let data  = ClientCreationData::try_from_slice(&signed.data[..]).unwrap();

      // handle IBC modules
      // write the client type, the client state, and the consensus state
      ibc::create_client(&data);
  }
  ```

- Transaction to transfer a token
  ```rust
  #[transaction]
  fn apply_tx(tx_data: Vec<u8>) {
      let signed =
          key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
      let data = PacketSendData::try_from_slice(&signed.data[..]).unwrap();

      // escrow the token, make a packet, and handle IBC modules
      ibc::transfer_send(&data);
  }
  ```

### Store IBC-related data
The IBC-related transaction can write IBC-related data to check the state or to be proved by other ledgers according to IBC protocol. Its storage key should be prefixed with `InternalAddress::Ibc` to protect them from other storage operations. The paths(keys) for Tendermint client are defined by [ICS 24](https://github.com/cosmos/ibc/blob/master/spec/core/ics-024-host-requirements/README.md#path-space). For example, a client state will be stored with a key `#IBC_encoded_addr/clients/{client_id}/clientState`.

### Emit IBC event
The ledger should set an IBC event to `events` in the ABCI response to allow relayers to get the events. We could add `IbcEvent` to `TxResult`. `IbcEvent` should have the IBC event type and necessary data according to the IBC operation. If the `IbcEvent` is set, the ledger sets an IBC event to the response of the transaction. `IbcEvent` should be given to `TxEnv` and a transaction should be able to set the data.

IBC relayer can subscribe to the ledger with Tendermint RPC or get the response when the relayer submits a transaction, then get the event. It is parsed in the relayer by [`from_tx_response_event()`](https://github.com/informalsystems/ibc-rs/blob/26087d575c620d1ec57b3343d1aaf5afd1db72d5/modules/src/events.rs#L167-L181).

```rust
/* apps/src/lib/node/ledger/protocol/mod.rs */

pub struct TxResult {
    pub gas_used: u64,
    pub changed_keys: HashSet<Key>,
    pub vps_result: VpsResult,
    pub initialized_accounts: Vec<Address>,
    pub ibc_event: IbcEvent,
}
```

```rust
/* shared/src/ledger/ibc/event.rs */

pub enum IbcEventType {
    NotIbcEvent,
    CreateClient,
    UpdateClient,
    SendPacket,
    ...
}

impl fmt::Display for IbcEvent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IbcEventType::NotIbcEvent => write!(f, "not_ibc_event"),
            IbcEventType::CreateClient => write!(f, "create_client"),
            ...
        }
    }
}

pub struct IbcEvent {
    pub event_type: IbcEventType,
    pub attributes: HashMap<String, String>,
}

impl IbcEvent {
    pub fn new() -> Self {
        IbcEvent {
            event_type: IbcEventType::NotIbcEvent,
            attributes: HashMap::new(),
        }
    }

    pub fn set_event_type(&mut self, event_type: IbcEventType) {
        self.event_type = event_type;
    }

    pub fn insert(&mut self, key: String, value: String) {
        self.attributes.insert(key, value);
    }
}
```

```rust
/* shared/src/vm/host_env.rs */

pub struct TxCtx<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Read-only access to the storage.
    pub storage: HostRef<'a, &'a Storage<DB, H>>,
    /// Read/write access to the write log.
    pub write_log: MutHostRef<'a, &'a WriteLog>,
    /// Storage prefix iterators.
    pub iterators: MutHostRef<'a, &'a PrefixIterators<'a, DB>>,
    /// Transaction gas meter.
    pub gas_meter: MutHostRef<'a, &'a BlockGasMeter>,
    /// The verifiers whose validity predicates should be triggered.
    pub verifiers: MutHostRef<'a, &'a HashSet<Address>>,
    /// IBC related data to be set to the tx event
    pub ibc_event: MutHostRef<'a, &'a IbvEvent>,
    /// Cache for 2-step reads from host environment.
    pub result_buffer: MutHostRef<'a, &'a Option<Vec<u8>>>,
}

...

/// IBC event type insertion function exposed to the wasm VM Tx environment.
pub fn tx_set_ibc_event_type<MEM, DB, H>(
    env: &TxEnv<MEM, DB, H>,
    val_ptr: u64,
    val_len: u64,
) -> TxResult<()>
where
    MEM: VmMemory,
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    let (event_type, gas) = env
        .memory
        .read_string(val_ptr, val_len as _)
        .map_err(|e| TxRuntimeError::MemoryError(Box::new(e)))?;
    tx_add_gas(env, gas)?;

    let event = unsafe { env.ctx.ibc_event.get() };
    event.set_event_type(event_type);
}

/// IBC data insertion function exposed to the wasm VM Tx environment.
pub fn tx_insert_ibc_attribute<MEM, DB, H>(
    env: &TxEnv<MEM, DB, H>,
    key_ptr: u64,
    key_len: u64,
    val_ptr: u64,
    val_len: u64,
) -> TxResult<()>
where
    MEM: VmMemory,
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    let (key, gas) = env
        .memory
        .read_string(key_ptr, key_len as _)
        .map_err(|e| TxRuntimeError::MemoryError(Box::new(e)))?;
    tx_add_gas(env, gas)?;
    let (value, gas) = env
        .memory
        .read_string(val_ptr, val_len as _)
        .map_err(|e| TxRuntimeError::MemoryError(Box::new(e)))?;
    tx_add_gas(env, gas)?;

    let event = unsafe { env.ctx.ibc_event.get() };
    event.insert(key, value);
}
```

### Handle IBC modules
IBC-related transactions should call functions to handle IBC modules. These functions are defined in [ibc-rs](https://github.com/informalsystems/ibc-rs) in traits (e.g. [`ClientReader`](https://github.com/informalsystems/ibc-rs/blob/d41e7253b997024e9f5852735450e1049176ed3a/modules/src/ics02_client/context.rs#L14)). But we can implement IBC-related operations (e.g. `create_client()`) without these traits because Anoma WASM transaction accesses the storage through the host environment functions.

```rust
/* shared/src/ledger/ibc/storage.rs */

/// Returns a key of the IBC-related data
pub fn ibc_key(path: impl AsRef<str>) -> Result<Self> {
    let path = Key::parse(path).map_err(Error::StorageKey)?;
    let addr = Address::Internal(InternalAddress::Ibc);
    let key = Key::from(addr.to_db_key());
    Ok(key.join(&path))
}

/// Returns a key for the client state
pub fn client_state_key(client_id: &ClientId) -> Key {
    let path = Path::ClientState(client_id.clone());
    ibc_key(path.to_string())
        .expect("Creating a key for the client state shouldn't fail")
}

...
```

```rust
/* vm_env/src/ibc.rs */

/// This struct integrates and gives access to lower-level IBC functions.
pub struct Ibc;

impl Ibc {
    pub fn create_client(data: &ClientCreationData) {
        let counter_key = client_counter_key().to_string();
        let counter = Self::get_and_inc_counter(&counter_key);
        let client_id = data.client_id(counter).expect("invalid client ID");
        // client type
        let client_type_key = client_type_key(&client_id).to_string();
        let client_type = data.client_state.client_type();
        tx::write(&client_type_key, client_type.clone());
        // client state
        let client_state_key = client_state_key(&client_id).to_string();
        tx::write(&client_state_key, data.client_state.clone());
        // consensus state
        let height = data.client_state.latest_height();
        let consensus_state_key =
            consensus_state_key(&client_id, height).to_string();
        tx::write(&consensus_state_key, data.consensus_state);

        // set the event type
        tx::set_ibc_event_type(ibc::CREATE_CLIENT_EVENT.to_owned());
        // set attributes
        tx::insert_ibc_attribute(CLIENT_ID.to_owned(), client_id.to_string());
        tx::insert_ibc_attribute(CLIENT_TYPE.to_owned(), client_type.to_string());
        tx::insert_ibc_attribute(CONSENSUS_HEIGHT.to_owned(), height.to_string());
    }
    ...
}
```

### Proof
If a proven IBC-related data is needed, the response of a query should have proof of the data (ICS 24). It is used to verify if the key-value pair exists or doesn't exist on the counterpart ledger in IBC validity predicate (ICS 23).

The query response has the proof as [`tendermint::merkle::proof::Proof`](https://github.com/informalsystems/tendermint-rs/blob/dd371372da58921efe1b48a4dd24a2597225df11/tendermint/src/merkle/proof.rs#L15), which consists of a vector of [`tendermint::merkle::proof::ProofOp`](https://github.com/informalsystems/tendermint-rs/blob/dd371372da58921efe1b48a4dd24a2597225df11/tendermint/src/merkle/proof.rs#L25). `ProofOp` should have `data`, which is encoded to `Vec<u8>` from [`ibc_proto::ics23::CommitmentProof`](https://github.com/informalsystems/ibc-rs/blob/66049e29a3f5a0c9258d228b9a6c21704e7e2fa4/proto/src/prost/ics23.rs#L49). The relayer getting the proof converts the proof from `tendermint::merkle::proof::Proof` to `ibc::ics23_commitment::commitment::CommitmentProofBytes` by [`convert_tm_to_ics_merkle_proof()`](https://github.com/informalsystems/ibc-rs/blob/66049e29a3f5a0c9258d228b9a6c21704e7e2fa4/modules/src/ics23_commitment/merkle.rs#L84) and set it to the request data of
 the following IBC operation.

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
                    // Use ClientReader functions to load the posterior state of modules

                    let client_id = get_client_id(key);
                    // Check the client state change
                    //   - created or updated
                    match check_client_state(client_id) {
                        StateChange::Created => {
                            // "CreateClient"
                            // Assert that the corresponding consensus state exists
                        }
                        StateChange::Update => {
                            match get_header(key, tx_data) {
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
                    // Use ConnectionReader functions to load the posterior state of modules

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
                    // Use ChannelReader functions to load the posterior state of modules

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
                            match get_proofs(tx_data) {
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
                    let packet = get_packet(key, tx_data)?;

                    // Use ChannelReader functions to load the posterior state of modules

                    // Assert that the packet sequence is the next sequence that the channel expects (Ordered channel)

                    // Assert that the commitment has been stored
                }

                "nextSequenceRecv" => {
                    // "RecvPacket"
                    let packet = get_packet(key, tx_data)?;

                    // Use ChannelReader functions to load the posterior state of modules

                    // Assert that the packet sequence is the next sequence that the channel expects (Ordered channel)

                    // Assert that the receipt and the ack have been stored
                }

                "nextSequenceAck" => {
                    // "Acknowledgement"
                    let packet = get_packet(key, tx_data)?;
                    let proofs = get_proofs(key, tx_data)?;

                    // Use ChannelReader functions to load the posterior state of modules

                    // Assert that the packet sequence is the next sequence that the channel expects (Ordered channel)

                    // Assert that the commitment has been deleted
                }

                "commitments" => {
                    let packet = get_packet(key, tx_data)?;
                    let proofs = get_proofs(key, tx_data)?;

                    // Use ChannelReader functions to load the posterior state of modules

                    // check if the commitment is stored or deleted
                    match check_commitment_state(key) {
                        StateChange::Deleted => {
                            // Assert that the packet was actually sent on this channel
                            //   - Get the stored commitment and compare it with a commitment made from the packet

                            // Check the channel state change
                            match get_channel_state(channel_id) {
                                ChannelState::Open => {
                                    // "AcknowledgementPacket"
                                    // Assert that the packet metadata matches the channel and connection information

                                    // Assert that the connection and channel are open

                                    // Verify that the packet was actually sent on this channel
                                    //   - Get the stored commitment and compare it with a commitment made from the packet

                                    // Verify the proofs to check the acknowledgement has been written on the counterpart ledger
                                    //   - Use `ibc-rs::ics04_connection::handler::verify::verify_packet_acknowledgement_proofs()`
                                }
                                ChannelState::Closed => {
                                    // Check the packet timeout
                                    if !is_timeout(packet) {
                                        // "TimeoutOnClose"
                                        // Verify the proofs to check the counterpart ledger's state is expected
                                        //   - The channel state on the counterpart ledger should be CLOSED
                                        //   - Use `ibc-rs::ics04_connection::handler::verify::verify_channel_proofs()`
                                    }
                                    // Verify the proofs to check the packet has not been confirmed on the counterpart ledger yet
                                    //   - For ordering channels, use `ibc-rs::ics04_connection::handler::verify::verify_next_sequence_recv()`
                                    //   - For not-ordering channels, use `ibc-rs::ics04_connection::handler::verify::verify_packet_receipt_absence()`
                                }
                                _ => return Err(Error::InvalidChannel("Invalid channel state")),
                            }
                        }
                        StateChange::Created => {
                            // "SendPacket"
                            // Assert that the packet metadata matches the channel and connection information
                            //   - the port is owend
                            //   - the channel exists
                            //   - the counterparty information is valid
                            //   - the connection exists

                            // Assert that the connection and channel are open

                            // Assert that the timeout height and timestamp have not passed on the destination ledger
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                "ports" => {
                    // check the state change
                    match check_port_state(key) {
                        StateChange::Created | StateChange::Updated => {
                            // check the authentication
                            self.authenticated_capability(port_id)?;
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                "receipts" => {
                    // Use ChannelReader functions to load the posterior state of modules

                    match check_state(key) {
                        StateChange::Created => {
                            let packet = get_packet(key, tx_data)?;
                            let proofs = get_proofs(key, tx_data)?;
                            // Assert that the receipt is valid

                            // Assert that the packet metadata matches the channel and connection information

                            // Assert that the connection and channel are open

                            // Assert that the timeout height and timestamp have not passed on the destination ledger

                            // Assert that the receipt and ack have been stored

                            // Verify the proofs that the counterpart ledger has stored the commitment
                            //   - Use `ibc-rs::ics04_connection::handler::verify::verify_packet_recv_proofs()`
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                "acks" => {
                    // Use ChannelReader functions to load the posterior state of modules

                    match check_state(key) {
                        StateChange::Created => {
                            // Assert that the ack is valid

                            // Assert that the receipt and ack have been stored
                        }
                        _ => return Err(Error::InvalidStateChange("Invalid state change happened")),
                    }
                }

                _ => return Err(Error::UnknownKeyPrefix("Found an unknown key prefix")),
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
IBC relayer monitors the ledger, gets the status, state and proofs on the ledger, and requests transactions to the ledger via Tendermint RPC according to IBC protocol. For relayers, the ledger has to make a packet, emits an IBC event and stores proofs if needed. And, a relayer has to support Anoma ledger to query and validate the ledger state. It means that `ChainEndpoint` in IBC Relayer of [ibc-rs](https://github.com/informalsystems/ibc-rs) should be implemented for Anoma like [that of CosmosSDK](https://github.com/informalsystems/ibc-rs/blob/master/relayer/src/chain/cosmos.rs). As those of Cosmos, these querys can request ABCI query to Anoma.

```rust
impl ChainEndpoint for Anoma {
    ...
}
```

## Transfer (ICS 20)
![transfer](./ibc/transfer.svg  "transfer")
