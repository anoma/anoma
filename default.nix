# NOTE the "anoma" and "wasm" top-level atrtibutes + the "features" can be
# removed when all code that use this default.nix  is adapted.
{ features ? [ "default" ], ... }:
let lf = (import (
  let
    lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  in fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash; }
) {
  src =  ./.;
}).defaultNix; in
lf // { inherit (lf.packages.${builtins.currentSystem}) anoma wasm; }
