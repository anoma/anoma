---
name: general-conventions
description: Language-agnostic code principles for live, inspectable systems.
---

# General Conventions

## Code Principles

- **Minimize code**: fewer lines, fewer branches, fewer special cases.
  Three similar lines is better than a premature abstraction, but
  duplicated logic across modules should be extracted.
- **Generalize, don't special-case**: if a solution works for one
  subsystem, make it work for all of them. Avoid subsystem-specific
  hacks when a uniform pattern exists.
- **Flatten control flow**: avoid deeply nested conditionals. Prefer
  early returns, pattern matching, or pipeline constructs that make
  the happy path obvious.
- **Dead code is noise**: if something is unused, remove it. Don't
  keep stubs, backwards-compat shims, or commented-out code.
- **Cross-module data should be typed structures**: not raw tuples,
  maps, or ad-hoc compound types. Define the shape once, document
  it, and use it everywhere.
- **Higher-level functions before lower-level helpers**: readers
  should encounter the "what" before the "how."
- **Unnecessary indirection is harmful**: trivial wrappers that just
  delegate to another module with no added value should be removed.

## Live Exploration

The system is designed to be live and inspectable. Use this.

- **Run examples** before writing or reviewing code. Don't guess at
  behavior from types and signatures alone — execute the code and
  observe actual state.
- **Inspect process/object state** at runtime to understand data
  flow and relationships between components.
- **Test hypotheses interactively**: "I ran X and observed Y" is
  stronger than "it appears Y might happen."
- **Trace actual call paths**: static search can miss dynamic
  dispatch. Running the system reveals actual usage.
- **Query persistent stores** (databases, tables, caches) to check
  if data is actually written, read, or both. This catches dead
  write paths that static analysis misses.

## Example-Driven Development

Prefer examples that live alongside production code over isolated
test files. Good examples:

- **Reveal something new** about the system. Each example should
  demonstrate a distinct behavior or property. Remove examples
  that show off the same behavior as another — redundancy obscures
  the interesting cases.
- Are runnable in the REPL without copy-paste or special setup.
- Return useful objects, not just pass/fail assertions.
- Can be composed — one example builds on another.
- Are type-checked and visible to tooling (LSP, static analysis).
- Double as both documentation and correctness checks.

## Implementation Anti-Patterns

Never:
- Add features beyond the request.
- Create abstractions for single use.
- Add "flexibility" or "configurability" not requested.
- Handle errors that can't happen.
- Improve adjacent code unprompted.

Test: every changed line traces to the request.

## Failure Protocol

1. **Stop** after 2 failed attempts at the same approach.
2. **Preserve state** — don't destroy partial work.
3. **Diagnose** — what failed and why.
4. **Capture** — note what to avoid and what to prefer.
5. **Decide** — change approach or ask for guidance.

Never brute-force. Never report success if verification failed.

## Learnings

Capture reusable knowledge when discovery cost >5 minutes and
the insight applies to future work. Two categories:

- **Avoid:** `[thing] — [why it failed] — [context]`
- **Prefer:** `[thing] — [why it works] — [context]`

Store in project memory files. Check learnings before starting
work on a familiar area. Remove learnings that turn out wrong.

## Architecture Awareness

- **Know which state is essential and which is derived.** Essential
  state is the data you can't regenerate. Derived state can be
  rebuilt from essential state. Don't protect derived state — if
  you can rebuild it, you don't need to back it up or migrate it.
- **Prefer platform primitives when they fit.** ETS over a database
  for ephemeral data, GenServer over a library for coordination,
  processes over threads. But reach for the right tool, not the
  ideological one.
- **Don't force a pattern.** Event sourcing suits some domains,
  plain CRUD suits others. The architecture should follow the
  problem, not a doctrine.

## Scope Awareness

Before starting work, briefly:
1. Restate what is needed.
2. Surface assumptions not stated.
3. Flag ambiguity.

This prevents wasted work on misunderstood objectives.

## Review Principles

When reviewing code, focus on architectural issues and unnecessary
complexity, not cosmetic nitpicks.

### Minimality Check

- Every changed line traces to a requirement.
- No abstractions for single-use code.
- No "flexibility" beyond spec.
- Could any code be removed while still satisfying requirements?

### Flag

- **Special-case logic** where a general solution exists.
- **Dead code**: defined but never called, written but never read.
  Verify with search AND runtime before flagging.
- **Wrong abstraction boundary**: logic in the wrong module, or a
  module doing too much.
- **Duplicated state**: data maintained in two places (e.g., a
  database and in-memory) without clear reason. Flag if only one
  side is actually consumed.
- **Duplicated logic**: same pattern in multiple places that should
  be a shared helper.
- **Verbose error handling**: rollback logic or defensive code for
  scenarios that can't happen.

### Don't Flag

- Formatting issues (that's what formatters are for).
- Missing docs on private/internal functions.
- Style preferences not in the project's style guide.
