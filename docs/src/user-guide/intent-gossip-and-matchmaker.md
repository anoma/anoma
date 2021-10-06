# The Intent gossip and Matchmaker

To run gossip node with intent gossip system and rpc server:

```shell
cargo run --bin anoma gossip --rpc "127.0.0.1:39111"
```

To run gossip node with intent gossip system, a token exchange matchmaker and RPC through which new intents requested:

```shell
cargo run --bin anoman gossip --rpc "127.0.0.1:39111" --matchmaker-path wasm/mm_token_exch.wasm --tx-code-path wasm/tx_from_intent.wasm --ledger-address "127.0.0.1:26657" --source matchmaker --signing-key matchmaker
```

This pre-built matchmaker implementation is [the fungible token exchange `mm_token_exch`](https://github.com/anoma/anoma/blob/master/wasm/wasm_source/src/mm_token_exch.rs), that is being used together with [the pre-built `tx_from_intent` transaction WASM](https://github.com/anoma/anoma/blob/master/wasm/wasm_source/src/lib.rs) to submit transaction from matched intents to the ledger.

## ✋ Example intents

1) We'll be using these addresses in the intents:

   ```shell
   export ALBERT=atest1v4ehgw368ycryv2z8qcnxv3cxgmrgvjpxs6yg333gym5vv2zxepnj334g4rryvj9xucrgve4x3xvr4
   export BERTHA=atest1v4ehgw36xvcyyvejgvenxs34g3zygv3jxqunjd6rxyeyys3sxy6rwvfkx4qnj33hg9qnvse4lsfctw
   export CHRISTEL=atest1v4ehgw36x3qng3jzggu5yvpsxgcngv2xgguy2dpkgvu5x33kx3pr2w2zgep5xwfkxscrxs2pj8075p
   export XAN=atest1v4ehgw36x3prswzxggunzv6pxqmnvdj9xvcyzvpsggeyvs3cg9qnywf589qnwvfsg5erg3fkl09rg5
   export BTC=atest1v4ehgw36xdzryve5gsc52veeg5cnsv2yx5eygvp38qcrvd29xy6rys6p8yc5xvp4xfpy2v694wgwcp
   export ETH=atest1v4ehgw36xqmr2d3nx3ryvd2xxgmrq33j8qcns33sxezrgv6zxdzrydjrxveygd2yxumrsdpsf9jc2p
   ```

2) Create files with the intents in JSON format:

   ```shell
   echo '[{"addr":"'$ALBERT'","key":"'$ALBERT'","max_sell":"300","min_buy":"50","rate_min":"0.7","token_buy":"'$BTC'","token_sell":"'$ETH'"}]' > intent.A.data

   echo '[{"addr":"'$BERTHA'","key":"'$BERTHA'","max_sell":"70","min_buy":"100","rate_min":"2","token_buy":"'$XAN'","token_sell":"'$BTC'","vp_path": "wasm_for_tests/vp_always_true.wasm"}]' > intent.B.data

   echo '[{"addr":"'$CHRISTEL'","key":"'$CHRISTEL'","max_sell":"200","min_buy":"20","rate_min":"0.5","token_buy":"'$ETH'","token_sell":"'$XAN'"}]' > intent.C.data
   ```

3) Instruct the matchmaker to subscribe to a topic "asset_v1":

   ```shell
   cargo run --bin anomac subscribe-topic --node "http://127.0.0.1:39111" --topic "asset_v1"
   ```

4) Submit the intents (the target gossip node must be running an RPC server):

   ```shell
   cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.A.data --topic "asset_v1" --signing-key Albert
   cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.B.data --topic "asset_v1" --signing-key Bertha
   cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.C.data --topic "asset_v1" --signing-key Christel
   ```

   The matchmaker should find a match from these intents and submit a transaction to the ledger that performs the n-party transfers of tokens.

## 🤝 Custom matchmaker

A custom matchmaker code can be built from [`wasm/mm_template`](https://github.com/anoma/anoma/tree/master/wasm/mm_template).

A matchmaker code must contain the following function, which will be called when a new intent is received:

```rust
use anoma_vm_env::matchmaker_prelude::*;

#[matchmaker]
fn add_intent(last_state: Vec<u8>, intent_id: Vec<u8>, intent_data: Vec<u8>) -> bool {
  // Returns a result of processing the intent
  true
}
```

The matchmaker can keep some state between its runs. The state can be updated from within the matchmaker code with [`update_state` function](https://docs.anoma.network/rustdoc/anoma_vm_env/imports/matchmaker/fn.update_state.html) and received from the `last_state` argument.

To find out about the interface available in a matchmaker and the library code used in the `mm_token_exch` implementation, please check out [Rust docs for `matchmaker_prelude`](https://docs.anoma.network/rustdoc/anoma_vm_env/matchmaker_prelude/index.html).
