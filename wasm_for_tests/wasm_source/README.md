# WASM source code in Rust

This crate contains WASM implementations of various transactions and validity predicates, used for testing.

## Quick start

```shell
# To be able to build this, make sure to have
make deps

# Build - this will output .wasm files in the parent dir
make all

# Each source that is included here can also be build and checked individually, e.g. for "tx_no_op" source:

make tx_no_op         # build
make check_tx_no_op   # cargo check
make test_tx_no_op    # cargo test
make watch_tx_no_op   # cargo watch
make clippy_tx_no_op  # cargo clippy
```
