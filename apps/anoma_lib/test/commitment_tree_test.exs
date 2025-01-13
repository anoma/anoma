defmodule CommitmentTreeTest do
  use ExUnit.Case, async: true
  alias Examples.ECommitmentTree

  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: ECommitmentTree

  doctest CommitmentTree
end
