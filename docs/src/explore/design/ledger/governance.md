# Governance

Anoma introduce a governance mechanism to propose and apply protocol changes with and without the need for an hard fork. Anyone holding some M1T will be able to prosose some changes to which delegators and validator will cast their yay or nay votes. Governance on Anoma supports both signaling and voting mechanism. The difference between the the two, is that the former is needed when the changes require an hard fork. In cases where the chain is not able to produce blocks anymore, Anoma relies an off chain signaling mechanism to agree on a common strategy.

## Governance & Treasury addresses

Governance introduce two internal address with their corresponding native vps:
- Governance address, which is in charge of validating on-chain proposals and votes
- Treasury address, which is in charge of holding treasury funds

Also, it introduces some protocol parameters:
- `min_proposal_fund`
- `max_proposal_code_size`
- `min_proposal_period`
- `max_proposal_content_size`
- `min_proposal_grace_epochs`
- `max_proposal_fund_transfer`

## On-chain proposals

On-chain proposals are created under the `governance_address` storage space and, by default, this storage space is initialized with following storage keys:
```
/$GovernanceAddress/counter: u64
```

In order to create a valid proposal, a transaction need to modify these storage keys:
```
/$GovernanceAddress/proposal/$id/content : Vec<u8>
/$GovernanceAddress/proposal/$id/author : Address
/$GovernanceAddress/proposal/$id/startEpoch: Epoch
/$GovernanceAddress/proposal/$id/endEpoch: Epoch
/$GovernanceAddress/proposal/$id/graceEpoch: Epoch
/$GovernanceAddress/proposal/$id/proposalCode: Option<Vec<u8>>
/$GovernanceAddress/proposal/$id/funds: u64
```

and follow these rules:
- `$id` must be equal to `counter + 1`.
- `startEpoch` must:
    - be grater than `currentEpoch`, where current epoch is the epoch in which the transaction is executed and included in a block
    - be a multiple of `min_proposal_period`.
- `endEpoch` must:
    - be at least `min_proposal_period` epoch greater than `startEpoch`
    - be a multiple of `min_proposal_period`
- `graceEpoch` must:
    - be at least `min_grace_epoch` epochs greater than `endEpoch`
- `proposalCode` can be empty and must be a valid transaction with size less than `max_proposal_code_size` kibibytes.
- `funds` must be at least `min_proposal_fund` and should be moved to the `governance_address`.
- `content` should follow the `Anoma Improvement Proposal schema` and must be less than `max_proposal_content_size` kibibytes.

A proposal gets accepted if, at least 2/3 of the total voting power (computed at the epoch definied in the `startEpoch` field) vote `yay`. If the proposal is accepted, the locked funds are returned to the address definied in the `proposal_author` field, otherwise are moved to the treasury address.

The `proposal_code` field can execute arbitrary code in the form of a wasm transaction. If the proposal gets accepted, the code is executed in the first block of the epoch following the `graceEpoch`.

Proposal can be submitted by any address as long as the above rules are respected. Votes can be casted only by active validators and delegator (at epoch `startEpoch` or less).
Moreover, validator can vote only during the first 2/3 of the voting period (from `startEpoch` and 2/3 of `endEpoch` - `startEpoch`).

The preferred content template (`Anoma Improvement Proposal schema`) is the following:

```json
{
    "title": "<string>",
    "authors": "<authors email addresses> ",
    "discussions-to": "<email address / link>",
    "created": "<date created on, in ISO 8601 (yyyy-mm-dd) format>",
    "license": "<abbreviation for approved license(s)>",
    "abstract": "<string>",
    "motivation": "<string>",
    "details": "<AIP number(s)> - optional field",
    "requires": "<AIP number(s)> - optional field",
}
```

## Off-chain proposal

In case where its not possibile to run a proposal online (for example, when the chain is halted), an offline mechanism can be used.
The ledger offers the possibility to create and sign proposal which are verified against a specific chain epoch.




