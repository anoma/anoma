# The Intent gossip and Matchmaker

To run an intent gossip node with an RPC server:

```shell
anoma node gossip --rpc "127.0.0.1:26660"
```

To run an intent gossip node with the intent gossip system, a token exchange matchmaker and RPC through which new intents are requested:

```shell
anoma node gossip --rpc "127.0.0.1:26660" --matchmaker-path libmm_token_exch --tx-code-path wasm/tx_from_intent.wasm --ledger-address "127.0.0.1:26657" --source matchmaker --signing-key matchmaker
```

Mind that `matchmaker` should be valid key in your wallet.

This pre-built matchmaker implementation is [the fungible token exchange `mm_token_exch`](https://github.com/anoma/anoma/blob/master/wasm/wasm_source/src/mm_token_exch.rs), that is being used together with [the pre-built `tx_from_intent` transaction WASM](https://github.com/anoma/anoma/blob/master/wasm/wasm_source/src/lib.rs) to submit transaction from matched intents to the ledger.

## ✋ Example intents

1) Lets create some accounts:
   ```
   anoma wallet key gen --alias alberto --unsafe-dont-encrypt
   anoma client init-account --alias alberto-account --public-key alberto --source alberto

   anoma wallet  key gen --alias christel --unsafe-dont-encrypt
   anoma client init-account --alias christel-account --public-key christel --source christel

   anoma wallet key gen --alias bertha --unsafe-dont-encrypt
   anoma client init-account --alias bertha-account --public-key bertha --source bertha
   
   anoma wallet key gen --alias my-matchmaker --unsafe-dont-encrypt
   anoma client init-account --alias my-matchmaker-account --public-key my-matchmaker --source my-matchmaker
   ```

1) We then need some tokens:

   ```
   anoma client transfer --source faucet --target alberto-account --signer alberto-account --token BTC --amount 1000
   anoma client transfer --source faucet --target bertha-account --signer bertha-account --token ETH --amount 1000
   anoma client transfer --source faucet --target christel-account --signer christel-account --token XAN --amount 1000
   ```

2) Lets export some variables:

   ```shell
   export ALBERTO=$(anoma wallet address find --alias alberto-account | cut -c 28- | tr -d '\n')
   export CHRISTEL=$(anoma wallet address find --alias christel-account | cut -c 28- | tr -d '\n')
   export BERTHA=$(anoma wallet address find --alias bertha-account | cut -c 28- | tr -d '\n')
   export XAN=$(anoma wallet address find --alias XAN | cut -c 28- | tr -d '\n')
   export BTC=$(anoma wallet address find --alias BTC | cut -c 28- | tr -d '\n')
   export ETH=$(anoma wallet address find --alias ETH | cut -c 28- | tr -d '\n')
   ```

3) Create files with the intents description:

   ```shell
   echo '[{"addr":"'$ALBERTO'","key":"'$ALBERTO'","max_sell":"70","min_buy":"100","rate_min":"2","token_buy":"'$XAN'","token_sell":"'$BTC'","vp_path": "wasm_for_tests/vp_always_true.wasm"}]' > intent.A.data
   
   echo '[{"addr":"'$BERTHA'","key":"'$BERTHA'","max_sell":"300","min_buy":"50","rate_min":"0.7","token_buy":"'$BTC'","token_sell":"'$ETH'"}]' > intent.B.data

   echo '[{"addr":"'$CHRISTEL'","key":"'$CHRISTEL'","max_sell":"200","min_buy":"20","rate_min":"0.5","token_buy":"'$ETH'","token_sell":"'$XAN'"}]' > intent.C.data
   ```

3) Start the ledger and the matchmaker. Instruct the matchmaker to subscribe to a topic "asset_v1":

   ```shell
   anoma node ledger run
   
   anoma node gossip --rpc "127.0.0.1:26660" --matchmaker-path wasm/mm_token_exch.wasm --tx-code-path wasm/tx_from_intent.wasm --ledger-address "127.0.0.1:26657" --source mm-1 --signing-key mm-1
   
   anoma client subscribe-topic --node "http://127.0.0.1:26660" --topic "asset_v1"
   ```

5) Submit the intents (the target gossip node must be running an RPC server):

   ```shell
   anoma client intent --data-path intent.A.data --topic "asset_v1" --signing-key alberto --node "http://127.0.0.1:26660"
   anoma client intent --data-path intent.B.data --topic "asset_v1" --signing-key bertha --node "http://127.0.0.1:26660"
   anoma client intent --data-path intent.C.data --topic "asset_v1" --signing-key christel --node "http://127.0.0.1:26660"
   ```

   The matchmaker should find a match from these intents and submit a transaction to the ledger that performs the n-party transfers of tokens.
   
6) You can check the balances with:

   ```
   anoma client balance --owner alberto-account
   anoma client balance --owner bertha-account
   anoma client balance --owner christel-account
   ```

## 🤝 Custom matchmaker

A custom matchmaker code can be built from [`matchmaker/mm_template`](https://github.com/anoma/anoma/tree/master/matchmaker/mm_template).

The `anoma_macros::Matchmaker` macro can be used to derive the binding code for the matchmaker runner on any custom implementation, e.g.:

```rust
#[derive(Default, Matchmaker)]
struct MyMatchmaker;
```

This macro requires that there is a `Default` implementation (derived or custom) for the matchmaker, which can be used by the runner to instantiate the matchmaker.

The matchmaker must also implement `AddIntent`, e.g.:

```rust
impl AddIntent for MyMatchmaker {
    // This function will be called when a new intent is received
    fn add_intent(
        &mut self,
        _intent_id: &Vec<u8>,
        _intent_data: &Vec<u8>,
    ) -> AddIntentResult {
        AddIntentResult::default()
    }
}
```

To submit a transaction from the matchmaker, add it to the `AddIntentResult` along with a hash set of the intent IDs that were matched into the transaction.
