defmodule AnomaTest.Communicator do
  use ExUnit.Case, async: true

  import Anoma.Node.Executor.Communicator

  alias Anoma.Node.Executor.Communicator

  alias Anoma.Subscriber.Basic

  alias Anoma.PartialTx
  alias Anoma.Node.Executor, as: Node
  doctest(Anoma.Node.Executor.Communicator)

  test "Proper Execution" do
    {:ok, supervisor} = Node.start_link(:p_exec)

    empty_tx = PartialTx.empty()

    successful_tx =
      empty_tx
      |> PartialTx.add_input(%Anoma.Resource{quantity: 2, logic: 0})

    failing_tx =
      empty_tx
      |> PartialTx.add_input(%Anoma.Resource{quantity: 2, logic: 1})

    assert Communicator.new_transactions(:p_exec_com, [empty_tx])

    assert Communicator.new_transactions(:p_exec_com, [
             empty_tx,
             successful_tx
           ])

    assert Communicator.new_transactions(:p_exec_com, [failing_tx]) == false

    assert Communicator.new_transactions(:p_exec_com, [
             failing_tx,
             successful_tx
           ]) == false

    Node.shutdown(supervisor)
  end
end
