# Get Elixir official image for build environment
FROM elixir:1.17 AS builder
# Get cmake in order to compile RocksDB
RUN apt-get update
RUN apt-get install -y cmake protobuf-compiler
# Install Rust in order to compile Cairo
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
# Add cargo to the path for future commands
ENV PATH="/root/.cargo/bin:${PATH}"
# Put the source code in the app directory
WORKDIR /app
# Copy the source material. Be sure that build artifacts are excluded because
# they may be incompatible with this image
COPY global_deps.exs mix.exs mix.lock version.exs ./
COPY apps apps
COPY config config
COPY documentation documentation
# The Hex package manager is required for Mix to fetch dependencies
RUN mix local.hex --force
# rebar3 is required to compile Anoma dependencies
RUN mix local.rebar --force
# Protocol Buffers compiler is also required
RUN mix escript.install hex protobuf 0.11.0 --force
# Add the Protocol Buffers compiler to the path
ENV PATH="/root/.mix/escripts:${PATH}"
RUN mix clean --deps
# Get all the dependencies required to build Anoma
RUN mix deps.get
# Finally, build an Anoma release
RUN mix release
# Create a minimal environment for running Elixir
FROM debian:bookworm-slim AS app
# OpenSSL is required for libcrypto
RUN apt-get update
RUN apt-get install -y openssl
# Copy the Anoma release into the minimal image
WORKDIR /app
COPY --from=builder /app/_build/dev/rel/anoma .
ENV LANG=C.UTF-8
# The entry point of the Anoma system
ENTRYPOINT ["bin/anoma"]
