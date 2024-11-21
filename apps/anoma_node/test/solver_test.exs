defmodule SolverTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.ESolver

  alias Anoma.Node.Examples.{ESolver, ENode}

  test "intentpool examples" do
    ESolver.solvable_transaction_via_intent_pool(
      ENode.start_node(node_id: "S_test3")
    )
  end
end
