# Actors and Incentives

Anoma consists of various actors fulfilling various roles in the network. They are all incentivized to act for the good of the network. The native Anoma token `XAN` is used to settle transaction fees and pay for the incentives in Anoma.

## Fees associated with a transaction

Users of Anoma can

- transfer private assets they hold to other users and
- barter assets with other users.

Each transaction may be associated with the following fees, paid in `XAN`:

- **Execution fees** to compensate for computing, storage and memory costs, charges at 2 stages:
  - **initial fee (init_f)**: charged before the transaction is settled
  - **post-execution fee (exe_f)**: charged after the settlement
- **Exchange fee (ex_f)**: a fee proportional to the value exchanged in a trade

## Actors and their associated fees and responsibilities

| Actor | Responsibilities  | Incentives  | Bond in escrow  | May also be  |
|---|---|---|---|---|
| User | Make offers or send transactions | Features of Anoma | X | Anyone |
| Signer  | Generate key shards  | portions of init_f, exe_f  | ✓  | Validator  |
| Validator  | Validate  | portions of init_f, exe_f  |✓   |  Signer |
| Submitter  | Submit orders & pay init_f  | successful orders get init_f back plus bonus  | X  |   |
| Intent gossip operator  | Signs and shares orders  | portions of init_f, exe_f  | X  |   |
| Market maker  | Signs and broadcast orders  | the difference between the ask and bid price | X | |
| Proposer | Proposes blocks | portions of init_f, exe_f | | Validator |

Questions to explore:

- How do we calculate the incentives? What are the equations for each actor?

- How do we calculate the bond/reward for the signers and validators?

- How do we ensure certain dual/multi agencies are allowed but not others? E.g., signers can be validators but we may not want them to be proposers because they may have knowledge of which transactions are encrypted.

## Actors and fees flowchart

![Summary](summary.png?raw=true)
