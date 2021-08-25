# Database

Important factors:
- persistent key/value storage
- reliability and efficiency (runtime performance and disk usage)
- thread safety
- ease of use

The considered DBs:
- LMDB
- LevelDB
- RocksDB
- sled - Rust native

To watch:
- [sanakirja](https://docs.rs/sanakirja) - too new to be considered for now, but has some [promising initial results](https://pijul.org/posts/2021-02-06-rethinking-sanakirja/) - TLDR. it can *fork tables* efficiently, it beats LMDB in benchmarks and usability

The current preference is for RocksDB as it's tried and tested. Eventually, we might want to benchmark against other backends for our specific use case.

## LMDB

<https://symas.com/lmdb/>

A compact and efficient, persistent in-memory (i.e. mmap-based) B+trees database. Reportedly has a great read performance, but not as good at writing.

Rust bindings:
- <https://github.com/mozilla/rkv>
- <https://github.com/AltSysrq/lmdb-zero>
- <https://github.com/vhbit/lmdb-rs> - some [comparison notes](https://github.com/vhbit/lmdb-rs/issues/32#issuecomment-310906601) with danburkert/lmdb-rs
- <https://github.com/danburkert/lmdb-rs>

## LevelDB

Log Structured Merge Tree db. Uses one global lock. Better write performance than LMDB and lower DB size.

Rust bindings:
- <https://github.com/skade/leveldb>

## RocksDB

A fork of LevelDB with different optimizations (supposedly for RAM and flash storage).

Used in <https://github.com/simplestaking/tezedge> and <https://github.com/near/nearcore>.

Rust bindings:
- <https://github.com/rust-rocksdb/rust-rocksdb>

## Sled

Repo: <https://github.com/spacejam/sled>
Homepage: <https://sled.rs/>

Modern, zero-copy reads, lock-free and many more features.

---

# Merkle tree data structure

Some popular choices for merkle tree in the industry are AVL(+) tree, Patricia Trie and Sparse Merkle Tree, each with different trade-offs.

AVL(+) tree is used in e.g. [Cosmos](https://github.com/cosmos/iavl). The advantage of this structure is that key don't need to be hashed prior to insertion/look-up.

Patricia trie used in e.g. [Ethereum](https://eth.wiki/en/fundamentals/patricia-tree) and [Plebeia for Tezos](https://www.dailambda.jp/blog/2020-05-11-plebeia/) are designed to be more space efficient.

Sparse Merle tree as described in [Optimizing sparse Merkle trees](https://ethresear.ch/t/optimizing-sparse-merkle-trees/3751) used in e.g. [Plasma Cash](https://ethresear.ch/t/plasma-cash-with-sparse-merkle-trees-bloom-filters-and-probabilistic-transfers/2006) are somewhat similar to Patricia trees, but perhaps conceptually simpler.

- Compact Sparse Merkle Trees <https://eprint.iacr.org/2018/955.pdf>
- Efficient Sparse Merkle Trees (caching) <https://eprint.iacr.org/2016/683.pdf>

Considered libraries:
- merk
- sparse-merkle-tree
- patricia_tree

## merk

<https://github.com/nomic-io/merk>

Using AVL tree built on top of RocksDB. It makes it easy to setup Merkle tree storage, but:
- is not yet fully implemented as described (e.g. [concurrent ops](https://github.com/nomic-io/merk/issues/26))
- benchmarks seem to differ from results in README
- doesn't have past states of the tree, instead [relies on RocksDB snapshot/checkpoint features](https://github.com/nomic-io/merk/blob/develop/docs/algorithms.md#database-representation), which means that it's [strongly coupled](https://github.com/nomic-io/merk/issues/11)
- uses a custom [encoding lib](https://github.com/nomic-io/ed) which is zero-copy, but big-endian everywhere
- there are a `unsafe` usages that are not well described/justified
- uses some experimental dep such as <https://github.com/rust-lang-nursery/failure> (now deprecated)

## sparse-merkle-tree

<https://github.com/jjyr/sparse-merkle-tree>

A nice abstraction, albeit not yet declared stable. It allows to plug-in a custom hasher function (which is important for [circuit friendliness](https://github.com/heliaxdev/rd-pm/issues/11)) and storage backend. Has minimal dependencies and support Rust `no_std`.

## patricia_tree

<https://github.com/sile/patricia_tree>
