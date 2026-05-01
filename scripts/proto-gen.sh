#!/usr/bin/env bash
set -euo pipefail

export PATH="$HOME/.mix/escripts:$PATH"
: "${PROTOC_GEN_ELIXIR:=$HOME/.mix/escripts/protoc-gen-elixir}"

command -v protoc >/dev/null || { echo "Missing protoc. Install: sudo apt install -y protobuf-compiler"; exit 1; }
[ -x "$PROTOC_GEN_ELIXIR" ] || { echo "Missing protoc-gen-elixir. Install: mix escript.install hex protobuf --force"; exit 1; }

echo "Using protoc: $(command -v protoc)"
echo "Using protoc-gen-elixir: $PROTOC_GEN_ELIXIR"

# örnek yollar; repo yapısına göre genişletilebilir
SRC_DIRS="apps/anoma_protobuf/priv/protos"
OUT_DIR="apps/anoma_protobuf/lib/anoma/protobuf"

mkdir -p "$OUT_DIR"
for d in $SRC_DIRS; do
  [ -d "$d" ] || continue
  find "$d" -name '*.proto' -print0 | xargs -0 -I{} \
    protoc --elixir_out=plugins=grpc:"$OUT_DIR" -I"$d" {}
done

echo "OK: protobuf code generated into $OUT_DIR"
