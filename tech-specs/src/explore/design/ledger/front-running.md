# Front-running prevention

tracking issue <https://github.com/heliaxdev/rd-pm/issues/28>

This page should describe how DKG can be integrated for front-running prevention.

## ABCI++ integration

The new API described in the ABCI++ proposal can be combined with a DKG for effective front-running protection.

### Prepare Proposal
* Include transactions encrypted to the shared public key that was generated in block `n-1`.

### Propose Block
* Initialise HybridDKG

### Process Proposal
* Check HybridDKG sends, Initialize HybridVSS

### Vote Extension
* Run HybridDKG
* Gossip decryption shares to decrypt transactions from block `n-1` that were encrypted to the shared public key that was generated in block `n-2`.

### Verify Vote Extension
* Finalize HybridDKG
* Decrypt transactions from block `n-1`

### Finalize Block
* Commit decrypted transactions from block `n-1`
* Commit shared public key from HybridDKG
* Commit evidence that HybridDKG finished
