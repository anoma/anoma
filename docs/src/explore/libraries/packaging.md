# Packaging

For Rust native code, cargo works great, but we'll need to package stuff from outside of Rust too (e.g. tendermint). The goal is to have a repo that can always build as is (reproducible) and easily portable (having a single command to install all the deps).

Options to consider:
- [nix packages](https://github.com/NixOS/nixpkgs)
- [guix](https://guix.gnu.org/manual/en/html_node/Package-Management.html)
- docker

## Cargo

For Rust dependencies, it would be nice to integrate and use:
- <https://github.com/crev-dev/cargo-crev>
- <https://github.com/rust-secure-code/cargo-geiger>
- <https://github.com/kbknapp/cargo-outdated>

## Nix

Purely functional package management for reproducible environment. The big drawback is its language.

## Guix

Similar package management capability to nix, but using scheme language.

## Docker

Not ideal for development, but we'll probably want to provide docker images for users.
