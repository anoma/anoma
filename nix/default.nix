{ config ? {}
, _cargoNix ? ../Cargo.nix
,
}:
let
  rustOverlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz");
  # rustc from rust-toolchain.toml
  rustChannel = (builtins.fromTOML (builtins.readFile ../rust-toolchain.toml)).toolchain.channel;
  # rustfmt, clippy, miri from nightly
  rustNightlyVersion = builtins.substring 8 10 (builtins.readFile ../rust-nightly-version);
  # nixpkgs-unstable 01-12-2021
  nixpkgs = let hash = "af21d41260846fb9c9840a75e310e56dfe97d6a3";
    in builtins.fetchTarball { url = "https://github.com/nixos/nixpkgs/archive/${hash}.tar.gz"; };
  overlays =
    [
      rustOverlay
      (self: super: {
        # Rust toolchains
        rustc = self.rust-bin.stable.${rustChannel}.minimal.override {
          targets = [ "x86_64-unknown-linux-gnu" "wasm32-unknown-unknown" ];
        };
        rustNightly = self.rust-bin.nightly.${rustNightlyVersion};
        # Wrapper which runs the pinned nightly version of cargo
        cargo-nightly = self.runCommandNoCC "cargo-nightly" { buildInputs = [ self.makeWrapper ]; } ''
          mkdir -p $out/bin
          makeWrapper ${self.rustNightly.cargo}/bin/cargo $out/bin/cargo-nightly \
            --prefix PATH : ${self.lib.makeBinPath [
              (self.rustNightly.default.override {
                targets = [ "x86_64-unknown-linux-gnu" "wasm32-unknown-unknown" ];
              })
            ]}
        '';
        # docs build needs this
        mdbook-linkcheck = self.callPackage ./mdbook-linkcheck.nix { };

        # you should generate Cargo.nix with "crate2nix generate" since it is not checked in to the repo
        cargoNix = import _cargoNix {
          pkgs = self;
          buildRustCrateForPkgs = pkgs: pkgs.buildRustCrate.override {
            defaultCrateOverrides = pkgs.callPackage ./crate-overrides.nix {};
          };
        };
      })
    ];

  pkgs = import nixpkgs {
    inherit overlays config;
  };

in pkgs
