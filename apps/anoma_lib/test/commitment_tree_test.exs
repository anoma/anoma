defmodule CommitmentTreeTest do
  alias Anoma.CommitmentTree
  alias Examples.ECommitmentTree

  use ExUnit.Case, async: true
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: ECommitmentTree

  doctest CommitmentTree
end
