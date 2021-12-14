package = anoma
version = $(shell git describe --dirty --broken)
platform = $(shell uname -s)-$(shell uname -m)
package-name = anoma-$(version)-$(platform)

bin = anoma anomac anoman anomaw

cargo := $(env) cargo
rustup := $(env) rustup
debug-env := RUST_BACKTRACE=1 RUST_LOG=$(package)=debug
debug-cargo := $(env) $(debug-env) cargo
# Nightly build is currently used for rustfmt and clippy.
nightly := $(shell cat rust-nightly-version)

# Path to the wasm source for the provided txs and VPs
wasms := wasm/wasm_source
# Paths for all the wasm templates
wasm_templates := wasm/tx_template wasm/vp_template wasm/mm_template wasm/mm_filter_template

# TODO upgrade libp2p
audit-ignores += RUSTSEC-2021-0076

build:
	$(cargo) build

build-abci-plus-plus:
	$(cargo) build --no-default-features --features "ABCI-plus-plus"

build-test:
	$(cargo) build --tests

build-test-abci-plus-plus:
	$(cargo) build --tests --no-default-features --features "ABCI-plus-plus"

build-release:
	$(cargo) build --release --package anoma_apps

check-release:
	$(cargo) check --release --package anoma_apps

package: build-release
	mkdir -p $(package-name)/wasm && \
	cd target/release && ln $(bin) ../../$(package-name) && \
	cd ../.. && \
	ln wasm/checksums.json $(package-name)/wasm && \
	tar -c -z -f $(package-name).tar.gz $(package-name) && \
	rm -rf $(package-name)

build-release-image-docker:
	docker build -t anoma-build - < docker/anoma-build/Dockerfile

build-release-docker: build-release-image-docker
	docker run --rm -v ${PWD}:/var/build anoma-build make build-release

package-docker: build-release-image-docker
	docker run --rm -v ${PWD}:/var/build anoma-build make package

check-wasm = $(cargo) check --target wasm32-unknown-unknown --manifest-path $(wasm)/Cargo.toml
check:
	$(cargo) check && \
	make -C $(wasms) check && \
	$(foreach wasm,$(wasm_templates),$(check-wasm) && ) true

check-abci-plus-plus:
	$(cargo) check --no-default-features --features "ABCI-plus-plus"

clippy-wasm = $(cargo) +$(nightly) clippy --manifest-path $(wasm)/Cargo.toml --all-targets -- -D warnings

clippy-wasm-abci-plus-plus = $(cargo) +$(nightly) clippy --manifest-path $(wasm)/Cargo.toml --all-targets --no-default-features --features "ABCI-plus-plus" -- -D warnings

clippy:
	$(cargo) +$(nightly) clippy --all-targets -- -D warnings && \
	make -C $(wasms) clippy && \
	$(foreach wasm,$(wasm_templates),$(clippy-wasm) && ) true

clippy-abci-plus-plus:
	$(cargo) +$(nightly) clippy --all-targets \
		--manifest-path ./apps/Cargo.toml \
		--no-default-features \
		--features "std testing ABCI-plus-plus" && \
	$(cargo) +$(nightly) clippy --all-targets --manifest-path ./proof_of_stake/Cargo.toml && \
	$(cargo) +$(nightly) clippy --all-targets \
		--manifest-path ./shared/Cargo.toml \
		--no-default-features \
		--features "testing ABCI-plus-plus" && \
	$(cargo) +$(nightly) clippy --all-targets \
		--manifest-path ./tests/Cargo.toml \
		--no-default-features \
		--features "wasm-runtime ABCI-plus-plus anoma_apps/ABCI-plus-plus" && \
	$(cargo) +$(nightly) clippy \
		--all-targets \
		--manifest-path ./vm_env/Cargo.toml \
		--no-default-features \
		--features "ABCI-plus-plus" && \
	make -C $(wasms) clippy && \
	$(foreach wasm,$(wasm_templates),$(clippy-wasm) && ) true

clippy-fix:
	$(cargo) +$(nightly) clippy --fix -Z unstable-options --all-targets --allow-dirty --allow-staged

install: tendermint
	ANOMA_DEV=false $(cargo) install --path ./apps

tendermint:
	./scripts/install/get_tendermint.sh

run-ledger:
	# runs the node
	$(cargo) run --bin anoman -- ledger run

run-ledger-abci-plus-plus:
	# runs the node
	$(cargo) run --bin anoman --no-default-features --features "ABCI-plus-plus" -- ledger run

run-gossip:
	# runs the node gossip node
	$(cargo) run --bin anoman -- gossip run

reset-ledger:
	# runs the node
	$(cargo) run --bin anoman -- ledger reset

reset-ledger-abci-plus-plus:
	# runs the node
	$(cargo) run --bin anoman --no-default-features --features "ABCI-plus-plus" -- ledger reset

audit:
	$(cargo) audit $(foreach ignore,$(audit-ignores), --ignore $(ignore))

test: test-unit test-e2e test-wasm

test-e2e:
	RUST_BACKTRACE=1 $(cargo) test e2e -- --test-threads=1

test-e2e-abci-plus-plus:
	RUST_BACKTRACE=1 $(cargo) test e2e \
		--manifest-path ./tests/Cargo.toml \
		--no-default-features \
		--features "wasm-runtime ABCI-plus-plus anoma_apps/ABCI-plus-plus" \
		-- --test-threads=1

test-unit-abci-plus-plus:
	$(cargo) test \
		--manifest-path ./apps/Cargo.toml \
		--no-default-features \
		--features "testing std ABCI-plus-plus" && \
	$(cargo) test --manifest-path ./proof_of_stake/Cargo.toml && \
	$(cargo) test \
		--manifest-path ./shared/Cargo.toml \
		--no-default-features \
		--features "testing ABCI-plus-plus" && \
	$(cargo) test \
		--manifest-path ./tests/Cargo.toml \
		--no-default-features \
		--features "wasm-runtime ABCI-plus-plus anoma_apps/ABCI-plus-plus" \
		-- --skip e2e && \
	$(cargo) test \
		--manifest-path ./vm_env/Cargo.toml \
		--no-default-features \
		--features "ABCI-plus-plus"

test-unit:
	$(cargo) test -- --skip e2e

test-wasm:
	make -C $(wasms) test

test-wasm-template = $(cargo) test --manifest-path $(wasm)/Cargo.toml
test-wasm-templates:
	$(foreach wasm,$(wasm_templates),$(test-wasm-template) && ) true

test-debug:
	$(debug-cargo) test -- --nocapture

fmt-wasm = $(cargo) +$(nightly) fmt --manifest-path $(wasm)/Cargo.toml
fmt:
	$(cargo) +$(nightly) fmt --all && \
	make -C $(wasms) fmt && \
	$(foreach wasm,$(wasm_templates),$(fmt-wasm) && ) true

fmt-check-wasm = $(cargo) +$(nightly) fmt --manifest-path $(wasm)/Cargo.toml -- --check
fmt-check:
	$(cargo) +$(nightly) fmt --all -- --check && \
	make -C $(wasms) fmt-check && \
	$(foreach wasm,$(wasm_templates),$(fmt-check-wasm) && ) true

watch:
	$(cargo) watch

clean:
	$(cargo) clean

build-doc:
	$(cargo) doc --no-deps
	make -C docs build

doc:
	# build and opens the docs in browser
	$(cargo) doc --open

build-wasm-image-docker:
	docker build -t anoma-wasm - < docker/anoma-wasm/Dockerfile

build-wasm-scripts-docker: build-wasm-image-docker
	docker run --rm -v ${PWD}:/usr/local/rust/wasm anoma-wasm make build-wasm-scripts

# Build the validity predicate, transactions, matchmaker and matchmaker filter wasm
build-wasm-scripts:
	make -C $(wasms)
	make opt-wasm
	make checksum-wasm

# need python
checksum-wasm:
	python3 wasm/checksums.py

# this command needs wasm-opt installed
opt-wasm:
	@for file in $(shell ls wasm/*.wasm); do wasm-opt -Oz -o $${file} $${file}; done

clean-wasm-scripts:
	make -C $(wasms) clean

publish-wasm:
	aws s3 sync wasm s3://heliax-anoma-wasm-v1 --acl public-read --exclude "*" --include "*.wasm" --exclude "*/*"

dev-deps:
	$(rustup) toolchain install $(nightly)
	$(rustup) target add wasm32-unknown-unknown
	$(rustup) component add rustfmt clippy miri --toolchain $(nightly)
	$(cargo) install cargo-watch

test-miri:
	$(cargo) +$(nightly) miri setup
	$(cargo) +$(nightly) clean
	MIRIFLAGS="-Zmiri-disable-isolation" $(cargo) +$(nightly) miri test

.PHONY : build check build-release clippy install run-ledger run-gossip reset-ledger test test-debug fmt watch clean build-doc doc build-wasm-scripts-docker build-wasm-scripts clean-wasm-scripts dev-deps test-miri
