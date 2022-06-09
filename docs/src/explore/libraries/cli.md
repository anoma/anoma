# Command-line interface

Important factors:
- UX
- ease of use
- cross-platform

The considered libraries:
- clap

## Clap

<https://github.com/clap-rs/clap>

Probably the most widely used CLI library in Rust.

With version 2.x, we'd probably want to update clap to 3.x: this version comes with deriving attributes and also other new ways to build CLI commands (previously, deriving was only provided by [StructOpt](https://github.com/TeXitoi/structopt), which is now in maintenance mode).
