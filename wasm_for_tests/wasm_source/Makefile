cargo := $(env) cargo
rustup := $(env) rustup
# Nightly build is currently used for rustfmt and clippy.
nightly := $(shell cat ../../rust-nightly-version)

# All the wasms that can be built from this source, switched via Cargo features
# Wasms can be added via the Cargo.toml `[features]` list.
wasms := tx_memory_limit
wasms += tx_mint_tokens
wasms += tx_no_op
wasms += tx_read_storage_key
wasms += tx_write_storage_key
wasms += vp_always_false
wasms += vp_always_true
wasms += vp_eval
wasms += vp_memory_limit
wasms += vp_read_storage_key
wasms += tx_proposal_code


# Build all wasms
all: 
	make $(wasms)
	make opt-wasm

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
$(wasms): %:
	$(cargo) build --release --target wasm32-unknown-unknown --features $@ && \
	cp "./target/wasm32-unknown-unknown/release/anoma_wasm_for_tests.wasm" ../$@.wasm

# `cargo check` one of the wasms, e.g. `make check_tx_no_op`
$(patsubst %,check_%,$(wasms)): check_%:
	$(cargo) check --target wasm32-unknown-unknown --features $*

# `cargo test` one of the wasms, e.g. `make test_tx_no_op`
$(patsubst %,test_%,$(wasms)): test_%:
	$(cargo) test --features $* \
		-- \
		-Z unstable-options --report-time

# `cargo watch` one of the wasms, e.g. `make watch_tx_no_op`
$(patsubst %,watch_%,$(wasms)): watch_%:
	$(cargo) watch --features $*

# `cargo clippy` one of the wasms, e.g. `make clippy_tx_no_op`
$(patsubst %,clippy_%,$(wasms)): clippy_%:
	$(cargo) +$(nightly) clippy --all-targets --features $* -- -D warnings

clean-wasm = rm ../$(wasm).wasm
clean:
	$(foreach wasm,$(wasms),$(clean-wasm) && ) true

deps:
	$(rustup) target add wasm32-unknown-unknown

# this command needs wasm-opt installed
opt-wasm:
	@for file in $(shell ls ../*.wasm); do wasm-opt -Oz -o $${file} $${file}; done

.PHONY : all check test clippy fmt fmt-check clean deps opt-wasm
