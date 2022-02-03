{ stdenv, nix-gitignore, mdbook, mdbook-mermaid, mdbook-linkcheck }:

stdenv.mkDerivation {
  name = "anoma-docs";
  src = nix-gitignore.gitignoreSource [ ] ../docs;
  buildInputs = [ mdbook mdbook-mermaid mdbook-linkcheck ];

  patchPhase = ''
    substituteInPlace src/specs/encoding.md \
      --replace ../../../proto ${../proto}
  '';

  buildPhase = ''
    make build
  '';

  installPhase = ''
    mkdir -p $out
    mv book/html $out/
  '';
}
