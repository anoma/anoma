# v0.11.0

- Anoma now has make file support
- [Docs now have a version for every release. No longer is base's logs only persisted](https://github.com/anoma/anoma/pull/276)
- [Router now has legible names](https://github.com/anoma/anoma/pull/293)
- [Router.call now has infinite timeout](https://github.com/anoma/anoma/pull/295)
- [Storage is now merkelized](https://github.com/anoma/anoma/pull/337)
- [Burrito used instead of escripts for the CLI](https://github.com/anoma/anoma/pull/316)
- [Anoma now has checkpointing](https://github.com/anoma/anoma/pull/327)

## Bug Fixes
- [Fixed a bug with keys<->binary in the nulifier public key](https://github.com/anoma/anoma/pull/292)
- [Remove Node.Storage.Ordering from subscribining to an Mnesia table](https://github.com/anoma/anoma/pull/304)
  + This caused random CI failures as it would get a message it was
    not prepared for.
- [Fix potential race condition of put](https://github.com/anoma/anoma/pull/323)
## Dependency Changes
- [rocksdb now uses the official rocksdb dependency](https://github.com/aeternity/mnesia_rocksdb/pull/51)
- [We now vendor libsodium](https://github.com/anoma/anoma/pull/282)
- [CI now does not depend on lee-dohm/generate-elixir-docs](https://github.com/anoma/anoma/pull/285)

## Inteneral Changes
- CI streamlining and abstraction
- The pinger can now be stopped
- Minor type fixups around the codebase
- Extra logging
- The Router now has more documentation
