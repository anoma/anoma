# This docker is used for deterministic wasm builds

# The version should be matching the version set in wasm/rust-toolchain.toml
FROM rust:1.61.0

WORKDIR /usr/local/rust/wasm

# The version should be matching the version set above
RUN rustup toolchain install 1.61.0 --profile minimal
RUN rustup target add wasm32-unknown-unknown

# Install nightly
# RUN rustup toolchain install nightly-2022-05-20 --profile minimal
# RUN rustup component add rustfmt --toolchain nightly-2022-05-20

# Download binaryen and verify checksum
ADD https://github.com/WebAssembly/binaryen/releases/download/version_109/binaryen-version_109-x86_64-linux.tar.gz /tmp/binaryen.tar.gz

# Extract and install wasm-opt
RUN tar -xf /tmp/binaryen.tar.gz
RUN mv binaryen-version_*/bin/wasm-opt /usr/local/bin
RUN rm -rf binaryen-version_*/ /tmp/binaryen.tar.gz
