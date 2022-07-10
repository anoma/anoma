# This docker is used for deterministic wasm builds

# The version should be matching the version set in wasm/rust-toolchain.toml
FROM rust:1.61.0

WORKDIR /usr/local/rust/wasm

# The version should be matching the version set above
RUN rustup toolchain install 1.61.0 --component rustc cargo rust-std rust-docs rls rust-analysis rustfmt
RUN rustup target add wasm32-unknown-unknown

# Download binaryen and verify checksum
ADD https://github.com/WebAssembly/binaryen/releases/download/version_101/binaryen-version_101-x86_64-linux.tar.gz /tmp/binaryen.tar.gz

# Extract and install wasm-opt
RUN tar -xf /tmp/binaryen.tar.gz
RUN mv binaryen-version_*/bin/wasm-opt /usr/local/bin
RUN rm -rf binaryen-version_*/ /tmp/binaryen.tar.gz
