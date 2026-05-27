---
name: code-review
description: Review code for architectural fit, dead code, and complexity. Use when asked to review a module, file, or PR.
---

# Code Review

Review the specified code using the code-reviewer agent.

## Instructions

1. Spawn the `code-reviewer` agent (from `.claude/agents/code-reviewer.md`)
   via the Task tool with subagent_type "general-purpose".
2. Pass it the file path or module name provided by the user.
3. Include in the prompt: "Follow the instructions in
   `.claude/agents/code-reviewer.md`. Review the following: [target]"
4. When the agent returns, present its findings to the user concisely.
   Group by severity: architectural issues first, then complexity,
   then style.
5. If the agent ran examples or queries interactively, include
   those observations — they're the strongest evidence.
