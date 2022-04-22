cargo := $(env) cargo
rustup := $(env) rustup
# Nightly build is currently used for rustfmt and clippy.
nightly := $(shell cat ../../rust-nightly-version)

# All the wasms that can be built from this source, switched via Cargo features
# Wasms can be added via the Cargo.toml `[features]` list.
wasms := tx_bond
wasms += tx_from_intent
wasms += tx_ibc
wasms += tx_init_account
wasms += tx_init_nft
wasms += tx_init_validator
wasms += tx_mint_nft
wasms += tx_transfer
wasms += tx_init_proposal
wasms += tx_unbond
wasms += tx_update_vp
wasms += tx_withdraw
wasms += tx_eth_bridge
wasms += vp_nft
wasms += vp_testnet_faucet
wasms += vp_token
wasms += vp_user

# Build all wasms
all: $(wasms)

# `cargo check` all wasms
check:
	$(foreach wasm,$(wasms),make check_$(wasm) && ) true

# `cargo test` all wasms
test:
	$(foreach wasm,$(wasms),make test_$(wasm) && ) true

# `cargo clippy` all wasms
clippy:
	$(foreach wasm,$(wasms),make clippy_$(wasm) && ) true

clippy-fix:
	$(cargo) +$(nightly) clippy --fix -Z unstable-options --all-targets --allow-dirty --allow-staged

fmt:
	$(cargo) +$(nightly) fmt --all

fmt-check:
	$(cargo) +$(nightly) fmt --all -- --check

# Build a selected wasm
# Linker flag "-s" for stripping (https://github.com/rust-lang/cargo/issues/3483#issuecomment-431209957)
$(wasms): %:
	RUSTFLAGS='-C link-arg=-s' $(cargo) build --release --target wasm32-unknown-unknown --features $@ && \
	cp "./target/wasm32-unknown-unknown/release/anoma_wasm.wasm" ../$@.wasm

# `cargo check` one of the wasms, e.g. `make check_tx_transfer`
$(patsubst %,check_%,$(wasms)): check_%:
	$(cargo) check --target wasm32-unknown-unknown --features $*

# `cargo test` one of the wasms, e.g. `make test_tx_transfer`
$(patsubst %,test_%,$(wasms)): test_%:
	$(cargo) test --features $*

# `cargo watch` one of the wasms, e.g. `make watch_tx_transfer`
$(patsubst %,watch_%,$(wasms)): watch_%:
	$(cargo) watch --features $*

# `cargo clippy` one of the wasms, e.g. `make clippy_tx_transfer`
$(patsubst %,clippy_%,$(wasms)): clippy_%:
	$(cargo) +$(nightly) clippy --all-targets --features $* -- -D warnings

clean-wasm = rm ../$(wasm).wasm
clean:
	$(foreach wasm,$(wasms),$(clean-wasm) && ) true

deps:
	$(rustup) target add wasm32-unknown-unknown

.PHONY : all check test clippy fmt fmt-check clean deps
