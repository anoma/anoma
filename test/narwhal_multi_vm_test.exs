defmodule NarwhalMultiVMTest do
  # Excluded by default (tag :multi_vm); each example spawns peer VMs, so
  # run serially in a NAMED BEAM (else spawning renames us and breaks
  # storage):  elixir --name primary@127.0.0.1 --cookie anoma -S mix test --include multi_vm
  use ExUnit.Case, async: false

  @moduletag :multi_vm
  @moduletag timeout: :timer.minutes(5)

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.ENarwhal.EMultiVM
end
