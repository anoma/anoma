# Domain name addresses

The transparent addresses are similar to domain names and the ones used in e.g. [ENS as specified in EIP-137](https://eips.ethereum.org/EIPS/eip-137) and [account IDs in Near protocol](https://nomicon.io/DataStructures/Account.html). These are the addresses of accounts associated with dynamic storage sub-spaces, where the address of the account is the prefix key segment of its sub-space.

A transparent address is a human-readable string very similar to a domain name, containing only alpha-numeric ASCII characters, hyphen (`-`) and full stop (`.`) as a separator between the "labels" of the address. The letter case is not significant and any upper case letters are converted to lower case. The last label of an address is said to be the top-level name and each predecessor segment is the sub-name of its successor.

The length of an address must be at least 3 characters. For compatibility with a legacy DNS TXT record, we'll use syntax as defined in [RFC-1034 - section 3.5 DNS preferred name syntax](https://www.ietf.org/rfc/rfc1034.txt). That is, the upper limit is 255 characters and 63 for each label in an address (which should be sufficient anyway); and the label must not begin or end with hyphen (`-`) and must not begin with a digit.

These addresses can be chosen by users who wish to [initialize a new account](#initializing-a-new-account), following these rules:

- a new address must be initialized on-chain
  - each sub-label must be authorized by the predecessor level address (e.g. initializing address `free.eth` must be authorized by `eth`, or `gives.free.eth` by `free.eth`, etc.) 
  - note that besides the address creation, each address level is considered to be a distinct address with its own dynamic storage sub-space and validity predicate.
- the top-level names under certain length (to be specified) cannot be initialized directly, they may be [auctioned like in ENS registrar as described in EIP-162](https://eips.ethereum.org/EIPS/eip-162).
  - some top-level names may be reserved

For convenience, the `anoma` top-level address is initially setup to allow initialization of any previously unused second-level address, e.g. `bob.anoma` (we may want to revise this before launch to e.g. auction the short ones, like with top-level names to make the process fairer).

Like in ENS, the addresses are stored on chain by their hash, encoded with [bech32m](https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki) ([not yet adopted in Zcash](https://github.com/zcash/zips/issues/484)), which is an improved version of [bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki). Likewise, this is for two reasons:
- help preserve privacy of addresses that were not revealed publicly and to prevent trivial enumeration of registered names (of course, you can still try to enumerate by hashes)
- using fixed-length string in the ledger simplifies gas accounting
