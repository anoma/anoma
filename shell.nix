with import ./nix { };

mkShell {
  buildInputs = [
    rustc
    rustNightly.rustfmt
    cargo-nightly
    clang
    llvmPackages.libclang
    protobuf
    crate2nix
    openssl
    # Needed at runtime
    tendermint
  ] ++ lib.optionals stdenv.isDarwin [ darwin.apple_sdk.frameworks.Security ];

  LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";
  PROTOC = "${protobuf}/bin/protoc";
  PROTOC_INCLUDE = "${protobuf}/include";
}
