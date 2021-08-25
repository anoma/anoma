# WASM runtime

Considered runtimes:
- wasmer
- wasmi

A good comparison overview is given in this [thread that discusses replacing wasmi with wasmer](https://forum.holochain.org/t/wasmi-vs-wasmer/1929) and its links. In summary:
- wasmer has native rust closures (simpler code)
- wasmer uses lexical scoping to import functions, wasmi is based on structs and trait impls
- the wasmer org maintains wasmer packages in many languages
- wasmer may be vulnerable to compiler bombs
  - this can be mitigated by using [a singlepass wasm compiler](https://lib.rs/crates/wasmer-compiler-singlepass-near)
- gas metering
  - wasmi inject calls to the host gas meter from Wasm modules
  - wasmer 
    - uses Middleware which injects the instructions at the parsing stage of the compiler (with inlining) - reduced overhead
    - must also consider compiler gas cost and how to handle compiler performance changes
  - it's hard to implement gas rules for precompiles
- [nondeterminism concerns](https://github.com/WebAssembly/design/blob/c9db0ebdee28d2f92726314c05cb8ff382701f8e/Nondeterminism.md)
  - different wasm versions (e.g. newly added features) have to be handled in both the compiled and interpreted versions
  - non-determinism in the source language cannot be made deterministic in complied/interpreted wasm either
  - threading - look like it has a long way to go before being usable
  - floats/NaN - can be avoided <https://github.com/WebAssembly/design/issues/582#issuecomment-191318866>
  - SIMD
  - environment resources exhaustion
- both are using the same spec, in wasmi words "there shouldn't be a problem migrating to another spec compliant execution engine." and "wasmi should be a good option for initial prototyping"
  - of course this is only true if we don't use features that are not yet in the spec

## wasmer

Repo: <https://github.com/wasmerio/wasmer>

Compiled with multiple backends (Singlepass, Cranelift and LLVM). It [support metering](https://github.com/wasmerio/wasmer/blob/3dc537cc49b8034047c3b142a66b3b6180f4447c/examples/metering.rs) via a [Middleware](https://github.com/wasmerio/wasmer/tree/3dc537cc49b8034047c3b142a66b3b6180f4447c/lib/middlewares).

## wasmi

Repo: <https://github.com/paritytech/wasmi>

Built for blockchain to ensure high degree of correctness (security, determinism). Interpreted, hence slower.

