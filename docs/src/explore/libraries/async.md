# Asynchronous programming

[Rust book on asynchronous programming](https://rust-lang.github.io/async-book/01_getting_started/01_chapter.html)

Rust does not incorporate a default runtime, and implementations are not
compatible with eachother.
c.f. <https://rust-lang.github.io/async-book/08_ecosystem/00_chapter.html#async-runtimes>

The three main one are async-std, futures and tokio.

## Tokio

Tokio is multithreaded, low cost and scalable. It also contains an async tcp &
udp socket and is used in both tendermint and libp2p.

<https://github.com/tokio-rs/tokio>
