#!/bin/bash
#
# Runs the test_node_connectivity e2e test (using ABCI++)
# $TENDERMINT must be set in the environment to the path to an ABCI++-enabled tendermint binary
set -euox pipefail
IFS=$'\n\t'

export RUST_BACKTRACE=short
export ANOMA_E2E_KEEP_TEMP=true
export ANOMA_E2E_DEBUG=true

export TM_LOG_LEVEL=debug
export ANOMA_LOG=debug

make build-wasm-scripts
make checksum-wasm

cargo test \
    e2e::ledger_tests::test_node_connectivity \
    --manifest-path ./tests/Cargo.toml \
    --no-default-features \
    --features "wasm-runtime ABCI-plus-plus anoma_apps/ABCI-plus-plus" \
    -- --test-threads=1 \
    --nocapture
