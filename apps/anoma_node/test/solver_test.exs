defmodule SolverTest do
  use ExUnit.Case, async: false

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.ESolver
end
