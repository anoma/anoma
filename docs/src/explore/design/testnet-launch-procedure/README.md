# Testnet Launch Procedure

## Desiderata

- Simple process for deploying new software versions to testnets
- Avoid debugging software on testnets (operationally expensive)
- Integrated _as much as possible_ into a regular engineering workflow
- Coordinating deployment with testnet operators
  - Clear communication of latest version & how to operate
- Written process (preflight checklist) to minimise chances of error

## Flavours of testnets

- Internal testnet (Heliax-only)
  - e.g. namada-internal-testnet-1
- Private (close-quarters) testnet (invite-only)
  - Private invite-only channel on Discord
  - Select group of participants (validators)
- Public testnet
  - Anyone can join, everything is public
  - Still coordinated on Discord

## Current process

1. Ray runs `anoma-network-init` through interactive prompts, get to a started network on cloud infrastructure, config files on Github.
    - Ask anyone else for help if necessary.
2. Alex R. updates, reads through, QA checks the testnet documentation for this version.
    - Ask anyone else for docs help if necessary.
3. Two-person signoff: Ray and Alex R. (necessary & sufficient)
4. Announce that the testnet is operational, provide the link to the latest documentation page.
