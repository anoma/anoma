#!/usr/bin/env bash

set -euo pipefail

echo Checking crate2nix...
nix run .#generateCargoNix
nix run .#generateCargoNixABCI-plus-plus
if ! git diff --exit-code --quiet crate2nix; then
	echo error: generated crate2nix files are not up to date in git >&2
	exit 1
fi

echo Checking flake...
nix flake check
nix flake show

echo Checking the Anoma program...
nix run 2>&1 | grep "Anoma command line interface"

echo Checking auxiliary WASM checksums output...
nix build .#wasm
[ "$(find -L result -type f)" = checksums.json ]

echo Checking docs build...
nix build .#anoma-docs
[ "$(ls result)" = html ]
