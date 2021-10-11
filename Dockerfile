FROM ubuntu:20.04
WORKDIR /var/build
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y \
    build-essential \
    libssl-dev \
    curl
RUN apt-get install -yq pkg-config
RUN apt-get install clang-tools-11 -y
RUN apt-get update
RUN apt-get clean
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup toolchain install 1.54.0 --component rustc cargo rust-std \
    rust-docs rls rust-analysis
