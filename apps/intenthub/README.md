# IntentHub

IntentHub is an intent registry and management system built on top of Anoma's intent-centric architecture. It allows users to register, track, and execute their Web3 intentions in a decentralized manner.

## Overview

IntentHub provides a simple yet powerful interface for managing user intents in the Web3 ecosystem. Instead of manually executing blockchain transactions, users can declare their intentions and let the system handle the execution when conditions are met.

## Features

- **Intent Registration**: Register various types of intents (NFT, Staking, Swap, DAO)
- **Intent Tracking**: Monitor the status of registered intents
- **Intent Execution**: Execute pending intents when conditions are met
- **User Management**: Track intents per user address
- **Statistics**: Get insights about registered intents
- **Type-based Filtering**: Filter intents by type

## Supported Intent Types

### NFT Intent
Register intent to purchase NFTs with specific conditions:
```elixir
parameters = %{
  contract_address: "0xabcdef1234567890",
  token_id: 1,
  max_price: 100
}
IntentHub.register_intent(user_address, :nft, parameters)
```

### Staking Intent
Register intent to stake tokens:
```elixir
parameters = %{
  token_address: "0xabcdef1234567890",
  amount: 1000,
  duration: 30
}
IntentHub.register_intent(user_address, :staking, parameters)
```

### Swap Intent
Register intent to swap tokens:
```elixir
parameters = %{
  token_in: "0xabcdef1234567890",
  token_out: "0xfedcba0987654321",
  amount_in: 100,
  min_amount_out: 95
}
IntentHub.register_intent(user_address, :swap, parameters)
```

### DAO Intent
Register intent to participate in DAO governance:
```elixir
parameters = %{
  dao_address: "0xabcdef1234567890",
  proposal_id: 42,
  vote_choice: :yes
}
IntentHub.register_intent(user_address, :dao, parameters)
```

## API Reference

### Core Functions

#### `register_intent/3`
Registers a new intent for a user.
```elixir
IntentHub.register_intent(user_address, intent_type, parameters, opts \\ [])
```

#### `list_user_intents/1`
Lists all intents for a specific user.
```elixir
IntentHub.list_user_intents(user_address)
```

#### `get_intent/1`
Retrieves an intent by its ID.
```elixir
IntentHub.get_intent(intent_id)
```

#### `execute_intent/1`
Executes a pending intent.
```elixir
IntentHub.execute_intent(intent_id)
```

#### `list_intents_by_type/1`
Lists all intents of a specific type.
```elixir
IntentHub.list_intents_by_type(intent_type)
```

#### `get_intent_stats/0`
Returns statistics about registered intents.
```elixir
IntentHub.get_intent_stats()
```

## Architecture

IntentHub is built using Anoma's intent-centric architecture and consists of:

- **IntentHub**: Main module providing the public API
- **IntentHub.Intent**: Intent struct and validation logic
- **IntentHub.IntentRegistry**: GenServer-based registry for intent storage and management

## Installation

IntentHub is part of the Anoma ecosystem. To use it:

1. Ensure you have Anoma installed and configured
2. Add IntentHub to your Anoma application dependencies
3. Start the IntentHub application

## Usage Example

```elixir
# Register an NFT purchase intent
user_address = "0x1234567890abcdef"
nft_params = %{
  contract_address: "0xabcdef1234567890",
  token_id: 1,
  max_price: 100
}

{:ok, intent} = IntentHub.register_intent(user_address, :nft, nft_params)

# List user's intents
intents = IntentHub.list_user_intents(user_address)

# Execute the intent when conditions are met
{:ok, executed_intent} = IntentHub.execute_intent(intent.id)

# Get statistics
stats = IntentHub.get_intent_stats()
```

## Testing

Run the test suite:

```bash
mix test
```

The test suite includes:
- Unit tests for Intent validation
- Integration tests for IntentRegistry
- End-to-end tests for the main IntentHub API

## Contributing

IntentHub follows Anoma's development guidelines and coding standards. When contributing:

1. Write comprehensive tests for new features
2. Follow Elixir best practices
3. Update documentation for API changes
4. Ensure compatibility with Anoma's intent-centric architecture

## License

IntentHub is part of the Anoma project and follows the same licensing terms.

## Future Enhancements

- Cross-chain intent execution
- Automated intent solving
- Advanced intent conditions and triggers
- Integration with external price feeds
- Intent composition and chaining
