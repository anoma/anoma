{ pkgs ? import ../nix { }
}:

with pkgs;

stdenv.mkDerivation {
  name = "anoma-docs";
  src = nix-gitignore.gitignoreSource [] ./.;
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
