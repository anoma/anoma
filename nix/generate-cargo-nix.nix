final: prev: {
  generateCargoNix = features:
    let
      fp = s: builtins.concatStringsSep "-" ([ s ] ++ builtins.filter (x: x != "default") features);
    in
    final.writeShellScriptBin "generateCargoNix" ''
      ${final.crate2nix}/bin/crate2nix generate --no-default-features \
        --features "${final.lib.concatStringsSep " " features}" \
        --crate-hashes crate2nix/${fp "crate-hashes"}.json \
        --output crate2nix/${fp "Cargo"}.nix \
        "$@"
    '';

  # Import appropriate Cargo.nix for requested features.
  cargoNixWith = features:
    let
      fp = s: builtins.concatStringsSep "-" ([ s ] ++ builtins.filter (x: x != "default") features);
    in
    import (../crate2nix/${fp "Cargo"}.nix) {
      rootFeatures = features;
      pkgs = final;
      inherit (final) buildRustCrateForPkgs;
    };

  buildRustCrateForPkgs = pkgs: pkgs.buildRustCrate.override {
    defaultCrateOverrides = pkgs.callPackage ./crate-overrides.nix { };
  };
}
