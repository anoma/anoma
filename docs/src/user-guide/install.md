# 💾 Install Anoma

## Requirements

- Building from source requires at least 16GB of ram available and 4 core cpu
- Free disk space of at least 60GB
- At the moment we are not supporting windows

There are different ways to install anoma.

## Source

Download the source code from [github anoma repo](https://github.com/anoma/anoma).
```shell
git clone https://github.com/anoma/anoma.git
```
To build from source you will have to install some dependecies:
- rust
- clang
- openssl
- git
- llvm

For ubuntu users the following command should install everything necessary:
```shell
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

apt-get install -y make git-core libssl-dev pkg-config libclang-12-dev
```

Last, to build and install anoma you can run the following command:

```bash
make install
```

## Github release

Go to [anoma github release page](https://github.com/anoma/anoma/releases) and download the last release.


## Docker

Go to [heliaxdev dockerhub account](https://hub.docker.com/r/heliaxdev/anoma) and pull the image.
