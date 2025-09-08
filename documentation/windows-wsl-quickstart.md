# Windows/WSL Quickstart (Elixir Umbrella)

## Prereqs
- Windows 10/11 + WSL2 (Ubuntu 22.04+)
- Git, build-essential, Erlang, Elixir

## Install on WSL Ubuntu
```bash
sudo apt update && sudo apt upgrade -y
sudo apt install -y git curl build-essential erlang elixir
elixir -v
mix --version

