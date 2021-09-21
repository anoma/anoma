# Epochs

An epoch is a range of blocks whose length is determined by the [epoch duration protocol parameter](./parameters.md#epoch-duration): minimum epoch duration and minimum number of blocks in an epoch. They are identified by consecutive natural numbers starting at 0.

We store the current epoch in global storage and the epoch of each block in the block storage. We also store the minimum height and minimum time of a first block in the next epoch in global storage, so that changes to the epoch duration protocol parameter don't affect the current epoch, but rather apply from the following epoch. Note that protocol parameters changes may themselves be delayed.

The first epoch (ID 0) starts on the genesis block. The next epoch minimum start time is set to the genesis time configured for the chain + minimum duration and the next epoch minimum height is set to the height of the genesis block (typically 1) + minimum number of blocks.

On each block `BeginBlock` Tendermint call, we check if the current epoch is finished, in which case we move on to the next epoch. An epoch is finished when both the minimum number of blocks and minimum duration of an epoch have been created from the first block of a current epoch. When a new epoch starts, the next epoch minimum height is set to the block's height + minimum number of blocks and minimum start time time is set to block's time from the block header + minimum duration.

## Predecessor blocks epochs

We store the epoch ranges of predecessor blocks. This is used for example for to look-up the epoch from an evidence of validators that acted maliciously (which includes block height and block time) for PoS system. For the PoS system, in block at height `h`, we only need to know values from Tendermint `max(h - consensus_params.evidence.max_age_num_blocks, 0)`, which is set to `100000` by default.

The predecessor epochs are stored in the block storage.  We update this structure on every new epoch and trim any epochs that ended more than `max_age_num_blocks` ago.
