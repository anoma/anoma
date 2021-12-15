# Intents

An intent is a way of expressing a user's desire. It is defined as arbitrary
data and an optional address for a schema. The data is as arbitrary as possible
to allow the users to express any sort of intent. It could range from defining a
selling order for a specific token to offering piano lessons or even proposing a
green tax for shoes’ manufacturers.

An intent is written using an encoding, or data schema. The encoding exists
either on-chain or off-chain. It must be known by users that want to express
similar intents. It also must be understood by some matchmaker. Otherwise, it 
possibly won’t be  matched. The user can define its own schema and inform either 
off-chain or on-chain. Having it on-chain allows it to easily share it with other
participants. Please refer to [data schema](./../ledger/storage/data-schema.md) for more
information about the usage of on-chain schema.

---

There is only a single intent type that is composed of arbitrary data and a
possible schema definition.

```rust
struct Intent {
    schema: Option<Key>,
    data: Vec<u8>,
    timestamp: Timestamp
}
```
