# v0.21.0

- [Normalize values going into storage](https://github.com/anoma/anoma/pull/791)
- [Added Shielded resource existence check](https://github.com/anoma/anoma/pull/788)
- [Have the TCP client preform exponential backoff](https://github.com/anoma/anoma/pull/802)
- [Bump the Cairo dependency such that dialyzer does not error](https://github.com/anoma/anoma/pull/825)

## CLI Changes

- [Fixed a bug where snapshotting would not give a proper error code to the client](https://github.com/anoma/anoma/pull/805)
- [The Erlang node will now phone back to the client when the Snapshot is done, allowing the client to wait until that is finished](https://github.com/anoma/anoma/pull/816)

## Documentation
- [Made diagrams for how the TCP Modules work](https://github.com/anoma/anoma/pull/814)

## Internal Changes
- [Fixed running storage examples multiple times](https://github.com/anoma/anoma/pull/758)
- [Added Logging messages to the TCP module](https://github.com/anoma/anoma/pull/777)

# v0.20.0

- The shielded RM is now merged.
- The event broker is now merged.

# v0.19.0

- Refactor and reformat various components to comply with the updated
  style guide.
- Add examples to the CLI modules.
- Store logs by the `encrypt` field rather than the ID.
- Store properly type-compliant nouns in logs.
- Correct the representation of shielded proof records.
- Support absent values for deleted keys in storage.
- Support silent CLI operation.
- Support truncated inspection of node transactions.

# v0.18.0
## Major Features
## Execution Environments
- [Add the Poseidon merkle tree specification to our merkle tree](https://github.com/anoma/anoma/pull/677)
- [Add binding signature verification in Cairo backend](https://github.com/anoma/anoma/pull/695)
- [Resource logic check in the Cairo backend](https://github.com/anoma/anoma/pull/696)
## CLI Changes
- [When submitting a resource machine transaction, nullifiers and commitments are printed](https://github.com/anoma/anoma/pull/641)
- [Add the ability to check for commitments in the CLi](https://github.com/anoma/anoma/pull/674)
  + Note that one could do this via the RO backend, but that is not ready yet.
- [Add the ability to check for nullifiers in the CLi](https://github.com/anoma/anoma/pull/675)
- [Read Only Cli Command](https://github.com/anoma/anoma/pull/681)
## Documentation
- [Add a glossary index for our codebase](https://github.com/anoma/anoma/pull/659)
## Bug Fixes
- [Fixed a bug where trying to use Juvix transactions would cause nock translation to fail](https://github.com/anoma/anoma/pull/500)
- [Made workers temporary rather than transient](https://github.com/anoma/anoma/pull/640)
  + this keeps the worker from spewing lots of noise on death or
    trying a restart loop.
- [Fixed the encoding of the empty to be 0 in atom transformation](https://github.com/anoma/anoma/pull/656)
  + This causes the failure of jamming transactions typically as [] is
    typically sent via the round-trip and fail.
- [Fix List Nock to Erlang to encode the empty list properly](https://github.com/anoma/anoma/pull/666)
- [Transactions via the CLI are now jammed before being sent over](https://github.com/anoma/anoma/pull/669)
  + This fixes a bug for larger contracts as they would hit the 1 MB
    transaction limit.
## Storage Changes
- [Add the hash(sha256) of the transparent commitment to storage](https://github.com/anoma/anoma/pull/634)
- [Add the hash(sha256) of the transparent nullifer to storage](https://github.com/anoma/anoma/pull/668)
- [Store the binary representation of the ID in storage](https://github.com/anoma/anoma/pull/658)
  + This is a nock compatible format, and useful when we completely
    normalize storage
## Debugging/Examples
- [Made Example Storage not cache](https://github.com/anoma/anoma/pull/662)
- [Fix the Dumper Example randomly dying due to trying to dump the wrong node](https://github.com/anoma/anoma/pull/680)
- A number of changes were had to make our CLI logic into examples
  + [Abstract out the CLI run dispatch logic](https://github.com/anoma/anoma/pull/682)
  + [Rename variables to be more sensible in the CLI](https://github.com/anoma/anoma/pull/683)
  + [Make the CLI runnable from IEX without exiting](https://github.com/anoma/anoma/pull/684)
  + [Add CLI examples](https://github.com/anoma/anoma/pull/689)
- [Use the Logging Engine in Router and Storage Engines](https://github.com/anoma/anoma/pull/687)
- [Add logging to the TCP Server](https://github.com/anoma/anoma/pull/692)
## Internal Changes
- [Added the ability to kill an engine](https://github.com/anoma/anoma/pull/639)
- [Adding type signatures to the mempool initiating](https://github.com/anoma/anoma/pull/642)
- [Add a custom transaction data types](https://github.com/anoma/anoma/pull/664)
- [Remove the order data structure](https://github.com/anoma/anoma/pull/648)
- [Added the ability to process jammed transactions](https://github.com/anoma/anoma/pull/665)
- [Reformat the Clock Engine to fit the style guide](https://github.com/anoma/anoma/pull/633)
- Added a reply to address for workers[[1]](https://github.com/anoma/anoma/pull/650)[[2]](https://github.com/anoma/anoma/pull/653)
- [Add a shielded transaction data structure](https://github.com/anoma/anoma/pull/678)
- [Moved commitment tree specs out of storage](https://github.com/anoma/anoma/pull/686)
- [better docs path creation](https://github.com/anoma/anoma/pull/693)

# v0.17.0
## Major Features

- [Abstract Addresses to be opaque](https://github.com/anoma/anoma/pull/512)
  + This makes addresses much nicer to work with and allows better
    changes in the future
- [Workers are engines](https://github.com/anoma/anoma/pull/513)

- [Cairo backend](https://github.com/anoma/anoma/pull/496)
  + This is a preview feature, as it does not currently check resource
    logics, however it does include a test showing it works with a compliance circuit
- [Read only backend](https://github.com/anoma/anoma/pull/597)
  + This is a read only backend. This is not enabled via the CLI yet,
    but is fully usable.

- Reformatting the codebase to comply with the [style guide](https://anoma.github.io/anoma/style-guide.html)
  + [Logger module](https://github.com/anoma/anoma/pull/580)
  + [config module](https://github.com/anoma/anoma/pull/579)
  + [dumper module](https://github.com/anoma/anoma/pull/578)
  + [pinger module](https://github.com/anoma/anoma/pull/577)
  + [mempool module](https://github.com/anoma/anoma/pull/610)
  + [ordering module](https://github.com/anoma/anoma/pull/615)
  + [executor module](https://github.com/anoma/anoma/pull/622)
  + [worker module](https://github.com/anoma/anoma/pull/623)

- Examplifying the codebase
  + [Examplifying clock](https://github.com/anoma/anoma/pull/598)
  + [Examplifying serialization](https://github.com/anoma/anoma/pull/587)
  + [Examplifying blocks](https://github.com/anoma/anoma/pull/582)
  + [Examplifying commitment trees](https://github.com/anoma/anoma/pull/583)
  + [Examplifying nouns](https://github.com/anoma/anoma/pull/631)
  + [Examplifying storage](https://github.com/anoma/anoma/pull/603)
  + [Examplifying pinger](https://github.com/anoma/anoma/pull/611)
  + [Examplifying intent](https://github.com/anoma/anoma/pull/605)
  + [Examplifying the identity machine](https://github.com/anoma/anoma/pull/606)
  + [Examplifying dumper](https://github.com/anoma/anoma/pull/613)




## Documentation

## Bug Fixes

### Typing Fixes
- [Fixing the type on getting namespace from storage](https://github.com/anoma/anoma/pull/627)
- [Fix the typing on the identity manager](https://github.com/anoma/anoma/pull/607)
  + They were incorrect and also overly constrained the type
- [We did not cover the full type range of adding namespaces](https://github.com/anoma/anoma/pull/625)

### Minor Bug Fixes
- [Properly update the counter on the dumper](https://github.com/anoma/anoma/pull/626)
- [Fix the Pinger Test Waiting](https://github.com/anoma/anoma/pull/630)

## Internal Changes
- [Pray Macro Improvements](https://github.com/anoma/anoma/pull/601)
- [Add a symbol utility module](https://github.com/anoma/anoma/pull/588)
- [Remove the intent module](https://github.com/anoma/anoma/pull/581)
- [Remove the write at functionality of storage](https://github.com/anoma/anoma/pull/595)

## Debugging/Helpers
- [Add the ability to get storage from `Anoma.Node`](https://github.com/anoma/anoma/pull/603)

# v0.16.0

## Major Features
- [We now have a new `debug` mode that makes tests pry on failure!](https://github.com/anoma/anoma/pull/525)
- [Resources kinds are now computed with the standard nock jam algorithm](https://github.com/anoma/anoma/pull/497)
- [Nullifiers now use a detatched signature](https://github.com/anoma/anoma/pull/503)
  + The format is now  `jam([jammed-nullified-resource detatched-signature])`.
    * I.E. jamming the resource and signing over the
      nullified-resource, and jamming both together
- [submit-rm client command](https://github.com/anoma/anoma/pull/519)
  + This means that RM backend submissions can be submitted via the cli!
- [New rational on testing](https://github.com/anoma/anoma/pull/558)
  + This means we will now focus on providing examples over future
    tests. Later work will come to deprecate the old testing
    documents, and write new ones about examples instead.
    * A big thanks to [JExample](https://scg.unibe.ch/research/jexample) and [Glamorous Toolkit](https://gtoolkit.com/) for inspiration.
- [New Style guide covering what every contirbutor must know if they want their changes accepted!!!](https://github.com/anoma/anoma/pull/575)
- Examples are now in the codebase! [[1](https://github.com/anoma/anoma/pull/560)] [[2](https://github.com/anoma/anoma/pull/561)] [[3](https://github.com/anoma/anoma/pull/564)] [[4](https://github.com/anoma/anoma/pull/565)]
  + Not all tests have been converted but the resource and nock files
    have been with work started on the node as well.

## Documentation
- For the more important documentation changes see the `Major Features` section.
- [Add documentation on some of the configuration logic](https://github.com/anoma/anoma/pull/534)

## Bug Fixes
- [Fixed random test failure](https://github.com/anoma/anoma/pull/570)
  + This was caused by 2 issues.
    1. The file name could contain the `\` character
    2. The code would not properly wait for the socket file to be
       created as it was asynchronous, this was mitigated by making
       sure the transport responds back
- [Fix bug where the configuration type was improperly typed, made more clear by refactoring](https://github.com/anoma/anoma/pull/501)
- [Bignums can now successfully be sent by our serialization](https://github.com/anoma/anoma/pull/552)
- [Fix double insertion for the same nullifier set](https://github.com/anoma/anoma/pull/559)
  + This bug would occur when we don't scry in a resource transaction,
    as the check happens for the nullifier doesn't want to see the
    complete nullifier set.
- [Fix a bug where the atom representation would change the specifics of jamming](https://github.com/anoma/anoma/pull/551)
  + This would cause some issues in kind calculation depending on
    where we ran it, [causing interesting errors](https://github.com/anoma/anoma/issues/550).
- [Fix TOC generation](https://github.com/anoma/anoma/pull/556)
  + There were two issues. The first being going up 2 levels caused
    the TOC to be improperly generated. Worse is that spacing was not
    considered for the markdown format when numbering got past 10.
    * [Seems that this even affected livebook itself!](https://github.com/livebook-dev/livebook/issues/2659)

## Internal Changes
- Minor typing improvements [[1](https://github.com/anoma/anoma/pull/520)] [[2](https://github.com/anoma/anoma/pull/527)] [[3](https://github.com/anoma/anoma/pull/540)]
- [Minor style improvements](https://github.com/anoma/anoma/pull/486)
- [Freeing up unused code and cleaning out the Node folder](https://github.com/anoma/anoma/pull/516)
- [Pinger now follows CQRS principles](https://github.com/anoma/anoma/pull/515)
- Minor Code Improvements [[1](https://github.com/anoma/anoma/pull/535)] [[2](https://github.com/anoma/anoma/pull/573)] [[3](https://github.com/anoma/anoma/pull/544)] [[4](https://github.com/anoma/anoma/pull/541)]
- Made functions that start the node take options instead of adhoc arguments [[1](https://github.com/anoma/anoma/pull/545)] [[2](https://github.com/anoma/anoma/pull/542)]
- [Made tests respect that they are tests and not overwrite the sockets produced in prod or dev mode](https://github.com/anoma/anoma/pull/536)

## Testing/Examples/Debugging
- [The socket created by the network test now properly has socket in the name](https://github.com/anoma/anoma/pull/523)
- [Logging now prints messages even without the logging engine](https://github.com/anoma/anoma/pull/568)
- [Fixup the block name in the transport engine tests](https://github.com/anoma/anoma/pull/553)
- [Fixup the path for the dumper in modes that aren't testing](https://github.com/anoma/anoma/pull/557)

# v0.15.0

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
