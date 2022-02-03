final: prev: {
  # docs build needs this
  mdbook-linkcheck = final.callPackage ./mdbook-linkcheck.nix { };

  anoma-docs = final.callPackage ./anoma-docs.nix { };

  # wasm checkksums.json
  wasm = final.runCommand "anoma-wasm" { } ''
    mkdir -p $out/wasm
    cp ${../wasm/checksums.json} $out/wasm/checksums.json
  '';
}
