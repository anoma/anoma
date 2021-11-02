# 💾 Install Anoma

There's a single command to build and install Anoma executables from source (the node, the client and the wallet). This command will also verify if a compatible version of Tendermint is available, and if not, attempt to install it. Note that currently at least 16GB RAM is needed to build from source.

```bash
make install
```

But prior to execution the command, some additional dependencies may be required. For example on Ubuntu 20.04 you need to run `sudo apt-get update && sudo apt-get install -y make git-core libssl-dev pkg-config libclang-12-dev` before starting the main build command.
