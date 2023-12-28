defmodule AnomaTest.Communicator do
  use ExUnit.Case, async: true

  doctest(Anoma.Node.Executor.Communicator)

  test "Proper Execution" do
    # {:ok, supervisor} = Node.start_link(:p_exec)
    # Node.shutdown(supervisor)
  end
end
