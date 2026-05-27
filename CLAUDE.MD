# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

@.claude/skills/general-conventions/SKILL.md
@.claude/skills/elixir-conventions/SKILL.md
@.claude/skills/git-conventions/SKILL.md

## Build & Development Commands

```bash
mix compile                    # compile all apps
mix test                       # run all tests
mix test --only juvix          # juvix-specific tests
mix test --only zk             # zk tests (excluded by default)
mix format                     # format code (78 char line length)
mix format --check-formatted   # check formatting
mix credo --strict             # lint (strict mode)
mix dialyzer                   # type checking
iex -S mix                     # interactive shell with full app loaded
```

Run one-off expressions without IEx:
```bash
timeout 60 mix run -e 'IO.puts("hello")'   # do NOT use --no-halt
```

## Architecture

Elixir umbrella app with 4 main apps under `apps/`:

- **anoma_node** - Core node: transaction processing, intents, transport. Entry point: `Anoma.Node`
- **anoma_lib** - Shared library: Nock interpreter, crypto, resource logic. No OTP application
- **anoma_client** - Client proxy (Phoenix HTTP + gRPC) between third parties and nodes
- **anoma_protobuf** - Protocol buffer definitions and gRPC services
- **event_broker** - Standalone PubSub with filter-based subscriptions

### Node Supervision Tree

```
Anoma.Supervisor
├── Registry
├── GRPC.Server.Supervisor
└── DynamicSupervisor (NodeSupervisor)
    └── Anoma.Node.Supervisor (per node, one_for_all)
        ├── Transport.Supervisor (proxies, network register)
        ├── Transaction.Supervisor (mempool, ordering, executor, storage, shards)
        ├── Intents.Supervisor (intent pool, solver)
        └── Logging
```

Each node has a unique `node_id` string. Process lookup uses `Anoma.Node.Registry` with `{node_id, engine, label}` addressing:
- `Registry.via(node_id, Module)` or `Registry.via(node_id, Module, label)` for registration
- `Registry.whereis(node_id, Module, label)` for lookup

### Mnesia

Used for persistent state (tables defined in `Anoma.Tables`). Table names are per-node atoms derived from `node_id`. Mnesia with optional RocksDB backend; disk persistence off by default.

## Git Workflow

Uses a git/git-style workflow (see `documentation/contributing/git.livemd`):

- **`base`** - Base all new work here (not main)
- **`main`** - Release preparation branch
- **`next`** - Integration branch with superset of features for next release
- **Topic branches** - Named `yourname/feature`, always branched from `base`
- Bug fix topics should be based on the commit that introduced the bug
- Never include unrelated changes in commits

## Code Conventions

### Documentation Style

First-person voice: "I am the Storage Engine." / "I return the shard ID."

Modules must have:
- `@moduledoc` starting with one-sentence purpose
- `### Public API` section listing all public functions
- All public functions documented and typed

Actor (GenServer) modules must have three sections:
```elixir
############################################################
#                      Public RPC API                      #
############################################################

############################################################
#                    GenServer Behavior                    #
############################################################

############################################################
#                  GenServer Implementation                #
############################################################
```

### Struct Rule

All data structures crossing module boundaries must be structs, not raw tuples/maps. Use `typedstruct` for definitions.

### CQRS

GenServer `call` only for reads or synchronization. Writes use `cast`. The `handle_call` body should be `{:reply, do_foo(...), state}` with logic in `do_foo`.

### Startup Options Pattern

Modules that take keyword options at startup define a `startup_options` type:
```elixir
@type startup_options ::
        {:node_id, String.t()}
        | {:id, atom()}

@spec start_link(list(startup_options())) :: GenServer.on_start()
```

### Examples Over Tests

Prefer examples in `lib/examples/e_<module>.ex` over tests in `test/`. Examples:
- Are available in IEx without copy-paste
- Can be composed and reused across other examples
- Are type-checked by Dialyzer
- Return useful objects, not just assertions
- Use `import ExUnit.Assertions` for `assert`

Example modules are named `E<Module>` (e.g., `ENode`, `EShard`, `EShardSupervisor`).

### Interactive Testing

Inspect process state via Registry:
```elixir
Registry.via(node_id, Shard, :a) |> :sys.get_state()
```

### Code Organization

Higher-level functions before lower-level helpers (Rule 1.4). Never call a callback from another callback (Rule 1.7).
