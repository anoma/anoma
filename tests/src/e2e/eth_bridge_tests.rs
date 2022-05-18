use std::fs;
use std::str::FromStr;

use anoma::ledger::eth_bridge::{self, storage};
use anoma::proto::{Signed, Tx};
use anoma::types::key::RefTo;
use anoma::types::transaction::protocol::ProtocolTxType;
use anoma::types::transaction::util::WriteOp;
#[cfg(not(feature = "ABCI"))]
use anoma_apps::tendermint_rpc::Client;
#[cfg(feature = "ABCI")]
use anoma_apps::tendermint_rpc_abci::Client;
use borsh::ser::BorshSerialize;
use eyre::{eyre, Context};

use crate::e2e::helpers::{get_actor_rpc, keys};
use crate::e2e::setup::constants::{
    wasm_abs_path, ALBERT, TX_WRITE_STORAGE_KEY_WASM,
};
use crate::e2e::setup::{Bin, Who};
use crate::e2e::{helpers, setup};
use crate::{run, run_as};

const LEDGER_STARTUP_TIMEOUT_SECONDS: u64 = 30;
const CLIENT_COMMAND_TIMEOUT_SECONDS: u64 = 30;
const SOLE_VALIDATOR: Who = Who::Validator(0);

#[test]
fn everything() {
    let test = setup::single_node_net().unwrap();

    let mut anoman_ledger = run_as!(
        test,
        SOLE_VALIDATOR,
        Bin::Node,
        &["ledger"],
        Some(LEDGER_STARTUP_TIMEOUT_SECONDS)
    )
    .unwrap();
    anoman_ledger
        .exp_string("Anoma ledger node started")
        .unwrap();
    anoman_ledger.exp_string("Tendermint node started").unwrap();
    anoman_ledger.exp_string("Committed block hash").unwrap();

    let ledger_addr = get_actor_rpc(&test, &SOLE_VALIDATOR);
    let tendermint_client =
        helpers::transactions::get_tm_websocket_client(&ledger_addr).unwrap();
    let queue_storage_key = storage::queue_key();
    let rt = tokio::runtime::Runtime::new().unwrap();
    let queue_abci_path = anoma::tendermint::abci::Path::from_str(&format!(
        "value/{}",
        queue_storage_key
    ))
    .unwrap();

    {
        println!(
            "Test a regular transaction that tries to modify #EthBridge/queue \
             is rejected"
        );
        let random_sk = keys::random_secret_key();

        let payload =
            b"this shouldn't end up being written to #EthBridge/queue".to_vec();
        let signed_payload = Signed::<Vec<u8>>::new(&random_sk, payload)
            .try_to_vec()
            .unwrap();
        let data_bytes = WriteOp {
            key: queue_storage_key.to_string(),
            value: signed_payload,
        }
        .try_to_vec()
        .unwrap();
        let data_path = test.base_dir.path().join("tx_data.bin");
        fs::write(&data_path, &data_bytes).unwrap();

        let tx_code_path = wasm_abs_path(TX_WRITE_STORAGE_KEY_WASM);
        let tx_code_path = tx_code_path.to_string_lossy();
        let tx_data_path = data_path.to_string_lossy().to_string();
        let mut anomac_args = vec![
            "tx",
            "--code-path",
            &tx_code_path,
            "--data-path",
            &tx_data_path,
            "--ledger-address",
            &ledger_addr,
        ];
        anomac_args.append(&mut vec!["--signer", ALBERT]);

        let mut anomac_tx = run!(
            test,
            Bin::Client,
            anomac_args,
            Some(CLIENT_COMMAND_TIMEOUT_SECONDS)
        )
        .unwrap();

        if !cfg!(feature = "ABCI") {
            anomac_tx.exp_string("Transaction accepted").unwrap();
        }
        anomac_tx.exp_string("Transaction applied").unwrap();
        anomac_tx.exp_string("Transaction is invalid").unwrap();
        anomac_tx
            .exp_string(&format!("Rejected: {}", eth_bridge::vp::ADDRESS))
            .unwrap();
        anomac_tx.assert_success();

        rt.block_on(async {
            let resp = tendermint_client
                .abci_query(Some(queue_abci_path), vec![], None, false)
                .await
                .unwrap();
            const EXPECTED_STR: &str = "No value found for key";
            assert!(
                resp.info.contains(EXPECTED_STR),
                "couldn't find {} in response: {:#?}",
                EXPECTED_STR,
                resp
            );
        });
    }

    {
        println!(
            "Test a protocol transaction signed by an active validator's \
             protocol key is accepted"
        );

        let wasm_dir = helpers::wasms::WasmDirectory::new(
            fs::canonicalize("../wasm").unwrap(),
        );
        let wasm_path =
            wasm_dir.get_abs_path("tx_enqueue_eth_transfer").unwrap();
        let wasm_bytes = fs::read(&wasm_path)
            .wrap_err_with(|| {
                eyre!("Couldn't read {}", wasm_path.to_string_lossy())
            })
            .unwrap();

        let protocol_sk = test.get_validator_protocol_sk(&SOLE_VALIDATOR);
        let protocol_pk = protocol_sk.ref_to();

        let wasm: Vec<u8> = wasm_bytes;
        let payload =
            b"this value gets written to #EthBridge/queue by this test"
                .to_vec();

        // we don't sign the whole tx but rather just the data part, as that's
        // the only part the VP can check
        let data_signed = Signed::new(&protocol_sk, payload.clone())
            .try_to_vec()
            .unwrap();
        let tx_unsigned = Tx::new(wasm, Some(data_signed));

        let protocol_tx_unsigned =
            ProtocolTxType::EthereumBridgeUpdate(tx_unsigned);
        // the signature of this outer tx (i.e. the protocol tx) is checked by
        // the ledger
        let protocol_tx = protocol_tx_unsigned.sign(&protocol_pk, &protocol_sk);

        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            tendermint_client
                .broadcast_tx_commit(protocol_tx.to_bytes().into())
                .await
                .unwrap();
        });

        anoman_ledger
            .exp_string("signature matches active validator")
            .unwrap();

        rt.block_on(async {
            let path = anoma::tendermint::abci::Path::from_str(&format!(
                "value/{}",
                queue_storage_key
            ))
            .unwrap();
            let resp = tendermint_client
                .abci_query(Some(path), vec![], None, false)
                .await
                .unwrap();
            let borsh_serialized = payload.try_to_vec().unwrap();

            assert_eq!(
                &borsh_serialized, &resp.value,
                "response value didn't match Borsh-serialized payload, got \
                 response: {:#?}",
                resp
            );
        });
    }
}
