# v0.14.0

- [Add the Transport engine into the router](https://github.com/anoma/anoma/pull/438)
  + This means that we can now message any engine from another engine
    with a signed message
- [Add functionality to Interact with Anoma from the Cli](https://github.com/anoma/anoma/pull/439)
  + This comes in two forms
    * Through the burrito binary (currently still a WIP)
    * Through mix tasks
  + The commands are
    * `get`
    * `submit`
    * [shutdown](https://github.com/anoma/anoma/pull/456)
    * [snapshot](https://github.com/anoma/anoma/pull/462)
    * [delete-dump](https://github.com/anoma/anoma/pull/462)
- [Add a Liscense to the project: MIT](https://github.com/anoma/anoma/pull/417)
- [Remove the suffix _dev, _prod to the anoma home directory (this may change again in the futrue)](https://github.com/anoma/anoma/pull/447)
- [Add a warning message and a command a user should run, if rocksdb grows out of sync](https://github.com/anoma/anoma/pull/452)
- [Configuration files are now loaded on startup and so is the default dump](https://github.com/anoma/anoma/pull/449)

## Nock
- [Jet binary operations (rsh, lsh, end, met, mix, and bex)](https://github.com/anoma/anoma/pull/430)
- [Add Sign and Verify Jets, along make layer 6 of the Nock standard library](https://github.com/anoma/anoma/pull/425)
- [Add jam and cue jets (implementation in Elixir with backreferences)](https://github.com/anoma/anoma/pull/451)


## Documentation
- [A livebook tutorial on our Logger](https://github.com/anoma/anoma/pull/427)
- [Add a document on how to run tests](https://github.com/anoma/anoma/pull/415)

## Bug Fixes
- [Fix Jets to properly run on atoms. Certain jets would not run on atoms that were backed by a binary value on the VM side](https://github.com/anoma/anoma/pull/437)
- [Fix a bug where the first snapshot was not namespaced, meaning contracts would not run without a hard storage reset](https://github.com/anoma/anoma/pull/444)

## Internal Changes
- [Replace xxhash with erlang-xxhash due to license reasons](https://github.com/anoma/anoma/pull/418)
- [Add helpers to help calculate the mug of Nock values](https://github.com/anoma/anoma/pull/424)
- [Storage now follows CQRS principles (global state mutation does casts, queries are calls)](https://github.com/anoma/anoma/pull/441)
- [Add child_specs to engines, and allow passthrough options to the kinds of restarts we wish the engines to have](https://github.com/anoma/anoma/pull/454)
- [Better debugging printers for various data types](https://github.com/anoma/anoma/pull/457)

# v0.13.0

- Follow XDG conventions for Anoma
  + Currently, we use *data_home* and *config_home* for Anoma folders
  + The folders are based on the mix environment, so having the *dev*
    folder will be put in *$XDG_DATA_HOME/anoma_dev/*
- RocksDB tables are now threaded throughout configuration, being
  turned on by default in the *prod* and *dev* environments
- An initial implementation of Jam is added to the Anoma standard library
- Storage is now properly namspaced by the router's id


## Internal Changes
- Storage is now an engine rather than a normal database handle

# v0.12.0

- Documentation release

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
