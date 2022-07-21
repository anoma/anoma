- Added WASM transaction and validity predicate `Ctx` with methods for host
  environment functions to unify the interface of native VPs and WASM VPs under
  `trait VpEnv` ([#1093](https://github.com/anoma/anoma/pull/1093))