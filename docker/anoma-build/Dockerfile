FROM ubuntu:20.04
ARG RUST_VERSION=1.61.0
WORKDIR /var/build
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y \
    build-essential \
    clang-tools-11 \
    curl \
    git \
    libssl-dev \
    pkg-config \
    && apt-get clean
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup toolchain install $RUST_VERSION --component \
    cargo \ 
    rls \
    rustc \
    rust-analysis \
    rust-docs \
    rust-std \
