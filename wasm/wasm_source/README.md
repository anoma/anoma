# WASM source code in Rust

This crate contains WASM implementations of various transactions and validity predicates.

## Quick start

```shell
# To be able to build this, make sure to have
make deps

# Build - this will output .wasm files in the parent dir
make all

# Each source that is included here can also be build and checked individually, e.g. for "tx_transfer" source:

make tx_transfer         # optimized build (strips `debug_log!` statements)
make debug_tx_transfer   # debug build
make check_tx_transfer   # cargo check
make test_tx_transfer    # cargo test
make watch_tx_transfer   # cargo watch
make clippy_tx_transfer  # cargo clippy
```
