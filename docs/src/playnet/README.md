# Playnet

 🕹🎮👾 Welcome to the very first Anoma testnet and thank you for joining us! 🕹🎮👾

The main goals of this testnet is to try out some of the functionality of the ledger, intent gossip and the matchmaker and to get some early feedback on its current state. To give feedback, ask questions and report issues, please use the #playnet Slack channel. Many issues and limitations are well known and our test coverage is currently very low, so please excuse Anoma while it is rough around the edges.

You can interact with Anoma via transactions, validity predicates and intents turned into transactions by the matchmaker. Because we don't have a proper wallet yet, each of us will have a pre-generated account address and a wallet key on the genesis block. Because all the keys are public, please respect others' keys and do not use them to sign stuff :)

## 💾 Install

You can either use the pre-built binaries we've released on Github or build from source. If you'll want to customize any of the wasm code (transactions, validity predicates and/or matchmaker), we recommend that you build from source to have the source available.

### Pre-built binaries

We have built for Mac (darwin_amd64) and Linux (linux_amd64). If you're not on one of these, you'll have to build from the source.

Download the release for your platform from <https://github.com/anoma/anoma/releases>. They are packaged with Tendermint, which will be used by the Anoma node.

```shell
# Extract the archive
tar -xf anoma_0.1_darwin_amd64.tar.gz

cd anoma

# Add the executables to your $PATH
export PATH="$(pwd):$PATH"
```

And you're ready to go!


### Build from the source

```shell
# Clone the repo
git clone https://github.com/anoma/anoma.git
# or
git clone git@github.com:anoma/anoma.git

cd anoma

# Checkout the release branch
git checkout v0.1-playnet

# Build and link the executables
make install
```

You'll also need to install Tendermint v0.34.* and have it available on your $PATH, e.g.:

```shell
apt install tendermint
nix-env -i tendermint
```

Or you can get it from <https://github.com/tendermint/tendermint/releases>.

To modify and build any of the wasm source codes:
```
# Run this first if you don't have Rust wasm target installed:
make -C txs/tx_template deps

# If you modify e.g. the transaction template (txs/template/src/lib.rs), you can build it with
make -C txs/tx_template
```

## 📇 Addresses

The following are the addresses that we have included in the genesis block. You can add them to your shell to use them in commands. You should be able to find your own address among them:

```shell
# User addresses
export adrian=a1qq5qqqqqxgeyzdeng4zrw33sxez5y3p3xqerz3psx5e5x32rg3rryw2ygc6yy3p4xpq5gvfnw3nwp8
export alberto=a1qq5qqqqq8yerw3jxx565y333gfpnjwzygcc5zd6xxarr2dzzgcm5xv3kxazrjve589p5vv34vl0yy3
export ash=a1qq5qqqqqxue5vs69xc6nwvfcgdpyy3pnxv6rxw2zx3zryv33gyc5xdekxaryydehgvunsvzz2hjedu
export awa=a1qq5qqqqqg565zv34gcc52v3nxumr23z9gezrj3pnx56rwse4xc6yg3phgcun2d33xyenqv2x4xyw62
export celso=a1qq5qqqqq8qmrwsjyxcerqwzpx9pnzve3gvc5xw29gdqnvv2yx5mrvsjpxgcrxv6pg5engvf5hgjscj
export chris=a1qq5qqqqqgye5xwpcxqu5z3p4g5ens3zr8qm5xv69xfznvwzzx4p5xwpkxc6n2v6x8yc5gdpeezdqc4
export gabriella=a1qq5qqqqq8ycn2djrxqmnyd3sxcunsv2zgyeyvwzpgceyxdf3xyu5gv2pgeprxdfe8ycrzwzzkezpcp
export gianmarco=a1qq5qqqqq89prqsf38qcrzd6zxym5xdfjg4pyg3pjg3pyx32zg5u5y3jpgc65zdej8pznwwf3jqzsws
export joe=a1qq5qqqqqgvuyv335g9z5v32xgdz523zxgsuy23fjxazrjve5g4pnydphxyu5v33cxarrzd692045xh
export nat=a1qq5qqqqq89rygsejx9q5yd6pxpp5x3f38ymyydp3xcu523zzx4prw3fc8qu5vvjpxyeyydpnfha6qt
export simon=a1qq5qqqqqgfqnqdecxcurq33hxcey2sf4g5mygdjyxfrrjse4xyc52vpjxyenwve4gv6njsecz4tzen
export sylvain=a1qq5qqqqqgccnyvp3gyergvp5xgmr2s3s8yung3f4gdq52wzpxvurysfhgycnwd29xfryxvekfwc00t
export tomas=a1qq5qqqqqggcrzsfj8ym5g3psxuurxv2yxseyxwpsxdpy2s35gsc5zdzpx9pyxde48ppnqd3cnzlava
export yuji=a1qq5qqqqqgvcrz3f5x4prssj9x5enydecxfznzdj9g5cnj3fcxarrxdjpx5cnwv69xye5vvfeva4z85

# Token Addresses
export XAN=a1qq5qqqqqxuc5gvz9gycryv3sgye5v3j9gvurjv34g9prsd6x8qu5xs2ygdzrzsf38q6rss33xf42f3
export BTC=a1qq5qqqqq8q6yy3p4xyurys3n8qerz3zxxeryyv6rg4pnxdf3x3pyv32rx3zrgwzpxu6ny32r3laduc
export ETH=a1qq5qqqqqxeryzvjxxsmrj3jpxapygve58qerwsfjxaznvd3n8qenyv2ygsc52335xue5vve5m66gfm
export XTZ=a1qq5qqqqqx3z5xd3ngdqnzwzrgfpnxd3hgsuyx3phgfry2s3kxsc5xves8qe5x33sgdprzvjptzfry9
export DOGE=a1qq5qqqqqx9rrq3zrg5myzv3eg9zyxvf3gery2dfhgg6nsdjrxscrgv6rgsunx33sxg6nvdjrkujezz
```

## ፨ The nodes

There are 4 ledger validator and intent gossip nodes running in cloud at:
- `52.210.23.30`
- `63.34.55.152`
- `54.195.72.213`
- `79.125.112.218`

The ledger is pre-configured to connect to them.

To run a local ledger node:
```shell
anoma run-ledger
```

To run the intent gossip with the matchmaker that can submit transactions to the local ledger:
```shell
anoma run-gossip --rpc "127.0.0.1:20202" \
  --matchmaker-path matchmaker_template/matchmaker.wasm \
  --tx-code-path txs/tx_from_intent/tx.wasm \
  --ledger-address "127.0.0.1:26657"
```

If you don't have a local ledger running, the matchmaker can also submit transactions to a remote validator ledger, with e.g.:
```shell
anoma run-gossip --rpc "127.0.0.1:20202" \
  --matchmaker-path matchmaker_template/matchmaker.wasm \
  --tx-code-path txs/tx_from_intent/tx.wasm \
  --ledger-address "52.210.23.30:26657"
```

## 🧮 WASM

Currently, Anoma only supports wasm built from Rust code. This is used for validity predicates, transactions' code, matchmaker's logic and matchmaker's intent filter. We provide a prelude with functions specialized for each of these environments. Additionally, any library code that you may attempt to use in wasm has to be able to compile to wasm, which means no foreign function interface (e.g. C dependencies).

Because wasm doesn't have any built-in logging facilities nor access to stdout, trying to print from wasm has no effect. Instead, we provide `log_string` function in the wasm environment preludes. When the wasm is being executed, this will be printed from the node's log (search for `WASM Transaction log`, `WASM Validity predicate log` or `WASM Matchmaker log`). You can use the Rust's `format!` macro for the string that you want to print. Because we don't yet have an RPC client, this in combination with transactions' `--dry-run` flag is the best way to query the ledger's state.

To view the full list of functions available in wasm, please refer to:
- [Transaction prelude](https://heliaxdev.github.io/anoma-playnet/doc/anoma_vm_env/tx_prelude/index.html)
- [Validity predicate prelude](https://heliaxdev.github.io/anoma-playnet/doc/anoma_vm_env/vp_prelude/index.html)
- [Matchmaker prelude](https://heliaxdev.github.io/anoma-playnet/doc/anoma_vm_env/matchmaker_prelude/index.html)

## ☑️ Validity predicates

Your account will be instantiated with a validity predicate built from the source code of `vps/vp_user/src/lib.rs` and a public key. This VP allows:
- for anyone to send you a token(s)
- for you to send a token(s), which must be authorized by a signature with the key associated with your account
- perform any changes to your account's storage, which must also be authorized by a signature
- perform transactions created by the matchmaker from intents that were signed by your the key associated with your account

You can customize this code and deploy it to the ledger with a transaction described below.

## 📩 Transactions

A transaction consists of a wasm code and additional optional arbitrary data bytes. The data will be passed to the transaction as its input when it's being executed on-chain.

Any of the following commands can optionally be submitted with `--dry-run` argument to simulate transaction's execution without including it in a block.

### 💸 A simple token transfer

To make a transfer of e.g. `10.1` of a fungible token `$XAN` from `$awa` to `$joe`:

```shell
anoma client transfer --source $awa \
  --target $joe \
  --token $XAN \
  --amount 10.1 \
  --code-path txs/tx_transfer/tx.wasm
```

This client command will take care of signing the transaction using your key.

To check balances of fungible token, run e.g.:

```shell
anoma client balances --address $adrian
```

### ♼ Update an account's validity predicate

To update an account's validity predicate, you can customize the default user's VP at `vps/vp_user/src/lib.rs`, or built your own from scratch using `vps/vp_template/src/lib.rs`. To deploy to, use e.g.:

```shell
# Compile the validity predicate
make -C vps/vp_user

# Submit a transaction with the updated VP to the ledger
anoma client update --address $awa --code-path vps/vp_user/vp.wasm
```

### 🦄 A custom transaction

To submit a custom transaction, one can use the `txs/tx_template/src/lib.rs` to implement the transaction's effects. Optionally, you can attach arbitrary bytes to the transaction from a file with the `--data-path` argument.

For example:

```shell
# Compile the transaction's code from the template
make -C txs/tx_template

# Submit the transaction to the ledger
anoma client tx --code-path txs/tx_template/tx.wasm --data-path tx.data
```

This transaction is by default not signed by any key, so if you try to use it make changes to your account's storage, it will be rejected by your validity predicate.

## ✋ Intents

In general, intents are some data that describe what you'd like to do with your account. We provide a template for intents signed by the source key of the source address, which can describe a desire to trade one of the fungible tokens.

To create a file `intent.data` with the intent's data, use e.g.:

```shell
anoma client craft-intent --address $awa \
  --token-buy $XTZ \
  --amount-buy 10 \
  --token-sell $BTC \
  --amount-sell 20 \
  --file-path intent.data
```

To submit the intent from the file to the intent gossip (which will propagate to matchmakers):
```shell
# Without a local intent gossip node, using one of the cloud nodes
anoma client intent --node "http://52.210.23.30:20202" \
  --data-path intent.data \
  --topic "asset_v0"

# With a local intent gossip node
anoma client intent --node "http://127.0.0.1:20202" \
  --data-path intent.data \
  --topic "asset_v0"
```

Once a matchmaker finds suitable matches of intents that it decides are likely to be accepted, it will try to submit a transaction to a connected ledger.

For now, to match intents, their amounts have to match exactly. In future, there will more options on how intents can be specified and matched.

## 🤝 Matchmaker

The provide matchmaker's code tries to match intents using a graph of intents in the format described above constructed with Tarjan's algorithm, but it also allows for you to write your own! The source code to the matchmaker is at `matchmaker_template/src/lib.rs`. To customize the transaction's code that is used to submit transactions from matched intent, you can update `txs/tx_from_intent/src/lib.rs`.

```
# build the matchmaker's code
make -C matchmaker_template

# build the matchmaker's transaction's code
make -C txs/tx_from_intent
```

And then restart the intent gossip with the matchmaker.
