# Serialization libraries

Because the serialization for the RPC and storage have different priorities, it might be beneficial to use a different library for each.

## RPC

Important factors:
- security, e.g.:
  - handling of malicious input (buffers should not be trusted)
  - secure RPC, if included (e.g. DoS or memory exhaustion vulnerabilities)
- native and cross-language adoption for easy interop
- ease of use
- reasonable performance

The considered libraries:
- protobuf
- cap'n'proto
- flatbuffers
- serde

The current preference is for protobuf using the prost library.

## Storage

Important factors:
- consistent binary representation for hashing
- preserve ordering (for DB keys)
- ease of use
- reasonable performance

The considered libraries:
- bincode
- borsh

## Protobuf

The most mature and widely adopted option. Usually combined with gRPC framework. The [Tendermint Rust ABCI](https://github.com/tendermint/rust-abci) provides protobuf definitions.

Implementations:
- <https://github.com/danburkert/prost> - Rust native
- <https://github.com/stepancheg/rust-protobuf> - Rust native
- <https://github.com/tafia/quick-protobuf> - [missing features](https://github.com/tafia/quick-protobuf/issues/12)

[A comparison of the two](https://www.reddit.com/r/rust/comments/czxny2/which_protocol_buffers_crates_to_invest_in/) main competing Rust implementations seems to favor Prost. Prost reportedly generates cleaner (more idiomatic) Rust code (<https://hacks.mozilla.org/2019/04/crossing-the-rust-ffi-frontier-with-protocol-buffers/#comment-24671>). Prost also has better performance (<https://github.com/danburkert/prost/issues/398#issuecomment-751600653>). It is possible to also add serde derive attributes for e.g. [JSON support](https://github.com/danburkert/prost/issues/75). JSON can be useful for development, requests inspection and web integration. However, to reduce attack surface, we might want to disallow JSON for write requests on mainnet by default.

gRPC implementations:
- <https://github.com/hyperium/tonic> - Rust native, using Prost and Tokio
- <https://github.com/tikv/grpc-rs> - build on C core library
- <https://github.com/stepancheg/grpc-rust> - not production ready

## Cap'n'proto
  
It avoids serialization altogether, you use the data natively in a representation that is efficient for interchange ("zero-copy"). The other cool feature is its ["time-traveling RPC"](https://capnproto.org/rpc.html). On the other hand concern for this lib is a much lower adoption rate, especially the Rust port which is not as complete. The format is designed to be safe against malicious input (on the both sides of a communication channel), but according to [FAQ](https://capnproto.org/faq.html) the reference impl (C++) has not yet undergone security review.

Implementations:
- <https://github.com/capnproto/capnproto-rust>

## Flatbuffers

Similar to protobuf, but zero-copy like Cap'n'proto, hence a lot faster.

Unfortunately, the Rust implementation is [lacking buffer verifiers](https://google.github.io/flatbuffers/flatbuffers_support.html), which is crucial for handling malicious requests gracefully. There is only draft implementation <https://github.com/google/flatbuffers/pull/6269>. This most likely rules out this option.

Implementations:
- <https://github.com/google/flatbuffers/tree/master/rust/flatbuffers>

## Serde

Serde is Rust native framework with great ergonomics. It supports many [different formats](https://serde.rs/#data-formats) implemented as libraries. It's used in some DBs too. Serde itself gives [no security guarantees](https://github.com/serde-rs/serde/issues/1087), handling of malicious input depends heavily on the used format. Serde can be used in combination with many other formats, like protobuf.

## Bincode

<https://github.com/servo/bincode>

Built on top of serde. Easy to use.

## Borsh

<https://github.com/near/borsh-rs>

Used in the Near protocol, it guarantees consistent representations and has a specification. It is also faster than bincode and is being [implemented in other languages](https://github.com/near/borsh#implementations).
