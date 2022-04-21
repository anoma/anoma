# Parameters

The parameters are used to dynamically control certain variables in the protocol. They are implemented as an internal address with a native VP. The current values are written into and read from the block storage in the parameters account's sub-space.

Initial parameters for a chain are set in the genesis configuration. On chain, these can be changed by 2/3 of voting power (specifics are TBA).

## Epoch duration

The parameters for [epoch](./epochs.md) duration are:

- Minimum number of blocks in an epoch
- Minimum duration of an epoch
- Maximum expected time per block 