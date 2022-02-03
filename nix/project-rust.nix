final: prev:
let
  targets = [ "x86_64-unknown-linux-gnu" "wasm32-unknown-unknown" ];
  nightlyVersion = builtins.head (builtins.match "([[:alnum:]-]*).*" (builtins.readFile ../rust-nightly-version));
  nightly = final.rust-bin.nightly.${builtins.substring 8 10 nightlyVersion};
in
{
  # Default rustc from rust-toolchain.toml
  rustc = (final.rust-bin.fromRustupToolchainFile ../rust-toolchain.toml).override { inherit targets; };

  # Nightly pinned to "../rust-nightly-version"
  rustc-nightly = nightly.default.override ({ inherit targets; });

  # Pull preferred components from the nightly to top-level
  inherit (nightly) rustfmt clippy miri;

  # Wrapper which runs the pinned nightly version of cargo
  using-nightly = final.writeShellScriptBin "using-nightly" ''
    PATH=${final.lib.makeBinPath [ final.rustc-nightly ]}:$PATH exec env "$@"
  '';

  # Emulate rustup: parse "cargo +nightly-2021-11-01 ..." and redirect to correc cargo from the overlay.
  cargoWrapper = final.writeShellScriptBin "cargo" ''
    if [[ "$1" = +nightly* ]]; then
      shift;
      PATH=${final.lib.makeBinPath [ final.rustc-nightly ]}:$PATH ${final.rustc-nightly}/bin/cargo "$@"
    else
      exec ${final.rustc}/bin/cargo "$@"
    fi
  '';
}
