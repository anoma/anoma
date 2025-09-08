#!/usr/bin/env bash
set -euo pipefail

echo "[check] Erlang/Elixir:"
which erl && erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
which elixir && elixir -v

echo "[check] protoc & plugins:"
which protoc || { echo "missing protoc"; exit 1; }
export PATH="$HOME/.mix/escripts:$PATH"
which protoc-gen-elixir || { echo "missing protoc-gen-elixir (run: mix escript.install hex protobuf --force)"; exit 1; }

echo "[deps] hex/rebar/deps:"
mix local.hex --force
mix local.rebar --force
mix deps.get

echo "[build] compile + test:"
mix compile
MIX_ENV=test mix test
