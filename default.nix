{ rust_overlay ? import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz")
, pkgs ? import <nixpkgs> {
    overlays = [
      rust_overlay

      (self: super: {
        rustc = pkgs.rust-bin.stable.latest.minimal.override {
          targets = [ "x86_64-unknown-linux-gnu" "wasm32-unknown-unknown" ];
        };
        inherit (super.rust-bin.stable.latest) rustfmt;
      })
    ];
  }
, lib ? pkgs.lib
, stdenv ? pkgs.stdenv
, xcbuild ? pkgs.xcbuild
}:
let
  crateOverrides = pkgs: with pkgs; {
      prost-build = attrs: { buildInputs = [ protobuf ]; };
      libp2p-core = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };
      libp2p-plaintext = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };
      libp2p-floodsub = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };
      libp2p-gossipsub = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };
      libp2p-identify = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };
      libp2p-kad = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };
      libp2p-relay = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };
      libp2p-noise = attrs: { buildInputs = [ protobuf ]; PROTOC = "${protobuf}/bin/protoc"; };

      # Additional build inputs are needed on OSX (they're using `xcrun`)
      blake2b-rs = attrs: { buildInputs = lib.optionals stdenv.isDarwin [ xcbuild ]; };
      wasmer-vm = attrs: { buildInputs = lib.optionals stdenv.isDarwin [ xcbuild ]; };

      librocksdb-sys = attrs: {
        buildInputs = [ clang rustfmt snappy lz4 zstd zlib bzip2 ]
          ++ lib.optionals stdenv.isDarwin [ xcbuild ];
        LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";
        # rust-rocksdb uses {}_LIB_DIR to determine whether it embeds compression libs in itself.
        # We need to tell it to use libs from the nix store so that we don't get linker errors later on.
        SNAPPY_LIB_DIR = "${snappy}/lib";
        LZ4_LIB_DIR = "${lz4}/lib";
        ZSTD_LIB_DIR = "${zstd}/lib";
        Z_LIB_DIR = "${zlib}/lib";
        BZ2_LIB_DIR = "${bzip2}/lib";
        # rust-rocksdb relies on CARGO_CFG_TARGET_FEATURE env variable
        # which is set by cargo. We are using rustc directly here, so
        # we need to set that variable.
        #
        # See: https://github.com/rust-rocksdb/rust-rocksdb/commit/81a9edea83473012378e808606cdd92c1212c076#diff-7377ccc9f5cb3386318bd8afcd84814e7dd6d9b40efa909fb2dc218ecb779499
        #
        # XXX empty string is the most portable, but it can't take
        # advantage of fast vectorization instructions on newer CPUs.
        CARGO_CFG_TARGET_FEATURE = "";
      };

      anoma = attrs: {
        buildInputs = [ rustfmt ];
        patchPhase = ''
          substituteInPlace build.rs --replace ./proto ${./proto}
        '';
      };

      anoma_apps = attrs: {
        buildInputs = [ rustfmt lz4 zstd zlib bzip2 ];
        patchPhase = ''
          substituteInPlace build.rs --replace ./proto ${./proto}
        '';
      };
    };

  # Generate Cargo.nix with: crate2nix generate
  cargo_nix = import ./Cargo.nix {
    inherit pkgs;
    buildRustCrateForPkgs = pkgs: pkgs.buildRustCrate.override {
      defaultCrateOverrides = pkgs.defaultCrateOverrides // crateOverrides pkgs;
    };
  };

in {
  apps = cargo_nix.workspaceMembers.anoma_apps;
}
