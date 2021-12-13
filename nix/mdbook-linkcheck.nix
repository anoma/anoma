{ rustPlatform, fetchFromGitHub, pkg-config, openssl }:

rustPlatform.buildRustPackage rec {
  pname = "mdbook-linkcheck";
  version = "0.7.5";

  src = fetchFromGitHub {
    owner = "Michael-F-Bryan";
    repo = "mdbook-linkcheck";
    rev = "5c6b338966250f1bed8539ef4b1e9c2d831d6ac0";
    sha256 = "0kvgcnh9hra2yzbqr93bcbahldy8l9vpclq9ki9mvk8pssngzrr8";
  };

  cargoSha256 = "0ia2dshh6kw3lihg2mqsa88f8gr8ig54khv1hsxpymizwgdnsrjm";

  dontCargoCheck = 1;

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ openssl ];

  OPENSSL_NO_VENDOR=1;
}
