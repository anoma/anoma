{ pkgs ? import ./nix { }
# Allows specifying Rust features for anoma crates.
, features ? ["std" "ABCI"]
# Allows overriding Tendermint derivation.
, tendermint ? pkgs.tendermint
, ... }:
let
  packages = rec {
    apps = pkgs.cargoNix.workspaceMembers.anoma_apps.build.override { inherit features; };

    # By default we want to have executables that have the correct "tendermint" and
    # "anoma*" executables available in PATH no matter how the user calls anoma etc.
    anoma = pkgs.runCommandNoCC "anoma" {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
      mkdir -p $out/bin
      for exe in ${apps}/bin/* ${tendermint}/bin/*; do
        makeWrapper $exe $out/bin/$(basename $exe) \
          --prefix PATH : ${apps}/bin:${tendermint}/bin
      done
    '';

    docs = pkgs.callPackage ./docs { };

    wasm = pkgs.runCommand "anoma-wasm" { } ''
      mkdir -p $out/wasm
      cp ${./wasm/checksums.json} $out/wasm/checksums.json
    '';
  };
in packages
