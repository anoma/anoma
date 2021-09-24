# The Intent gossip and Matchmaker

```bash
# run gossip node with intent gossip system and rpc server (use default config)
cargo run --bin anoma gossip --rpc "127.0.0.1:39111"

# run gossip node with intent gossip system, matchmaker and rpc (use default config)
cargo run --bin anoman gossip --rpc "127.0.0.1:39111" --matchmaker-path wasm/mm_token_exch.wasm --tx-code-path wasm/tx_from_intent.wasm --ledger-address "127.0.0.1:26657" --source matchmaker --signing-key matchmaker

# Prepare intents:
# 1) We'll be using these addresses in the intents:
export ALBERT=atest1v4ehgw368ycryv2z8qcnxv3cxgmrgvjpxs6yg333gym5vv2zxepnj334g4rryvj9xucrgve4x3xvr4
export BERTHA=atest1v4ehgw36xvcyyvejgvenxs34g3zygv3jxqunjd6rxyeyys3sxy6rwvfkx4qnj33hg9qnvse4lsfctw
export CHRISTEL=atest1v4ehgw36x3qng3jzggu5yvpsxgcngv2xgguy2dpkgvu5x33kx3pr2w2zgep5xwfkxscrxs2pj8075p
export XAN=atest1v4ehgw36x3prswzxggunzv6pxqmnvdj9xvcyzvpsggeyvs3cg9qnywf589qnwvfsg5erg3fkl09rg5
export BTC=atest1v4ehgw36xdzryve5gsc52veeg5cnsv2yx5eygvp38qcrvd29xy6rys6p8yc5xvp4xfpy2v694wgwcp
export ETH=atest1v4ehgw36xqmr2d3nx3ryvd2xxgmrq33j8qcns33sxezrgv6zxdzrydjrxveygd2yxumrsdpsf9jc2p
# 2) Create file containing the json representation of the intent:
echo '[{"addr":"'$ALBERT'","key":"'$ALBERT'","max_sell":"300","min_buy":"50","rate_min":"0.7","token_buy":"'$BTC'","token_sell":"'$ETH'"}]' > intent.A.data

echo '[{"addr":"'$BERTHA'","key":"'$BERTHA'","max_sell":"70","min_buy":"100","rate_min":"2","token_buy":"'$XAN'","token_sell":"'$BTC'","vp_path": "wasm_for_tests/vp_always_true.wasm"}]' > intent.B.data

echo '[{"addr":"'$CHRISTEL'","key":"'$CHRISTEL'","max_sell":"200","min_buy":"20","rate_min":"0.5","token_buy":"'$ETH'","token_sell":"'$XAN'"}]' > intent.C.data

# 3) Instruct the matchmaker to subscribe to new network:
cargo run --bin anomac subscribe-topic --node "http://127.0.0.1:39111" --topic "asset_v1"

# 4) Submit the intents (the target gossip node need to run an RPC server):
cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.A.data --topic "asset_v1" --signing-key Albert
cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.B.data --topic "asset_v1" --signing-key Bertha
cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.C.data --topic "asset_v1" --signing-key Christel
```
