# Ubuntu 16.04 LTS makes it possible to compile binaries that are compatible
# with GLIBC >= 2.23. Ubuntu 16.04 is supported until 2026.
FROM ubuntu:16.04
ARG RUST_VERSION=1.54.0
WORKDIR /var/build
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update \
    && apt-get install -y build-essential libssl-dev curl pkg-config \
		clang-tools-8 \
		&& apt-get clean
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup toolchain install $RUST_VERSION --component rustc cargo rust-std \
    rust-docs rls rust-analysis
