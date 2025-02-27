defmodule Anoma.CommitmentTreeTest do
  alias Anoma.CommitmentTree
  alias Anoma.Examples.ECommitmentTree

  use ExUnit.Case, async: true
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: ECommitmentTree

  doctest CommitmentTree
end
