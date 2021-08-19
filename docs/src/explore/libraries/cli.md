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

With version 2.x, we'd probably want to use it with [Structops](https://github.com/TeXitoi/structopt) for deriving.

But we can probably use 3.0, which is not yet stable, but is pretty close <https://github.com/clap-rs/clap/issues/1037>. This version comes with deriving attributes and also other new ways to build CLI commands.
