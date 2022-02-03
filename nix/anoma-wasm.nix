# This overlay should build the wasm files inside a Nix derivation, but it's broken
# because dependencies fail to vendor (building inside a derivation reqires that
# the set of dependencies is determined exactly before the build step even begins).
#
# It fails because cargo-vendor does not handle multiple versions (even if renamed)
# of a library in the same set of dependencies - and currently there's quite a
# few such dependencies. Other cargo commands and `crate2nix` do not seem to mind.
# The alternative Nix Rust build platforms (crate2nix, naersk, cargo2nix) have
# very poor support for cronss builds, though. And with the weird WASM build target
# in particulal.
#
# The WebAssembly build works in a shell environment just fine, so this isn't a
# huge issue.
final: prev: {
  anoma-wasm = final.callPackage
    ({ lib, pkgs, rustc }: final.rustPlatform.buildRustPackage {
      pname = "anoma_wasm";
      version = "0.0.0";

      src = ../.;

      buildAndTestSubdir = "wasm/wasm_source";

      buildNoDefaultFeatures = true;
      buildFeatures = [ ];
      cargoLock = {
        lockFile = ../Cargo.lock; # XXX or Cargo-ABCI-plus-plus.lock
        # crate2nix <-> rustPlaftorm translation
        outputHashes = with lib;
          mapAttrs'
            (a: b: {
              name =
                let x = builtins.match "([^ ]*) ([^ ]*) .*" a;
                in head x + "-" + concatStrings (tail x);
              value = b;
            })
            (importJSON ../crate2nix/crate-hashes.json);
      };

      nativeBuildInputs = [ rustc ];

      buildPhase = ''
        cargo build --release --target=wasm32-unknown-unknown
        mkdir -p $out/src
        cp wasm/wasm_source/*.wasm $out
      '';

      installPhase = "echo install phase skipped";
    })
    { };
}
