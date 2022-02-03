{
  description = "Anoma";

  inputs.rust-overlay.url = "github:oxalica/rust-overlay";
  inputs.nixpkgs.follows = "rust-overlay/nixpkgs";
  inputs.flake-utils.follows = "rust-overlay/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  # we need tendermint 0.34
  inputs.nixpkgs-tendermint.url = "nixpkgs/nixos-21.11-small";


  outputs = { self, nixpkgs, flake-utils, rust-overlay, flake-compat, nixpkgs-tendermint }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
    in

    with nixpkgs.lib;
    with flake-utils.lib;

    eachSystem supportedSystems (system:

      let
        overlays = [
          (import rust-overlay)
          (final: prev: { inherit (import nixpkgs-tendermint { inherit system; }) tendermint; })
        ] ++ import nix/overlays.nix;

        pkgs = import nixpkgs { inherit system overlays; };

        cargoNix = pkgs.cargoNixWith [ "default" ];
        cargoNix-ABCI-plus-plus = pkgs.cargoNixWith [ "ABCI-plus-plus" ];

        # By default we want to have executables that have the correct "tendermint" and
        # "anoma*" executables available in PATH no matter how the user calls `anoma`,
        # `anoma` calls `anoman`, `anoman` calls `tendermint` and so on.
        mkAnoma = apps: pkgs.runCommandNoCC "anoma" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
          mkdir -p $out/bin
          for exe in ${apps}/bin/* ${pkgs.tendermint}/bin/*; do
            makeWrapper $exe $out/bin/$(basename $exe) \
              --prefix PATH : ${apps}/bin:${pkgs.tendermint}/bin
          done
        '';
      in
      {
        defaultApp = self.apps.${system}.anoma;

        apps = {
          anoma = mkApp { drv = self.packages.${system}.anoma; };
          generateCargoNix = mkApp { drv = pkgs.generateCargoNix [ "default" ]; };
          generateCargoNixABCI-plus-plus = mkApp { drv = pkgs.generateCargoNix [ "ABCI-plus-plus" ]; };
        };

        devShell = with pkgs; pkgs.mkShell {
          packages = [
            cargoWrapper
            using-nightly
            rustfmt
            clippy
            miri
            rustc
            clang
            llvmPackages.libclang
            protobuf
            openssl
            # Needed to build WASM modules (provides `wasm-opt`)
            binaryen
            # Needed at runtime
            tendermint
          ] ++ lib.optionals stdenv.isDarwin [ darwin.apple_sdk.frameworks.Security ];
          LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";
          PROTOC = "${protobuf}/bin/protoc";
          PROTOC_INCLUDE = "${protobuf}/include";
        };

        defaultPackage = self.packages.${system}.anoma;

        packages = {
          anoma = mkAnoma cargoNix.workspaceMembers.anoma_apps.build;
          "anoma:ABCI-plus-plus" = mkAnoma cargoNix-ABCI-plus-plus.workspaceMembers.anoma_apps.build;

          inherit (pkgs) wasm anoma-docs;

          # wasm src build - broken
          #inherit (pkgs) anoma-wasm;
        }
        // mapAttrs' (n: v: nameValuePair ("rust_" + n) v.build) cargoNix.workspaceMembers
        // mapAttrs' (n: v: nameValuePair ("rust_" + n + ":ABCI-plus-plus") v.build) cargoNix-ABCI-plus-plus.workspaceMembers;
      });
}
