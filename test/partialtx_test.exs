defmodule AnomaTest.PartialTx do
  use ExUnit.Case, async: true

  alias Anoma.Communicator

  alias Anoma.Subscriber.Basic

  alias Anoma.PartialTx

  doctest(Anoma.PartialTx)

  test "indeed we are balanced! Or not!?!?!" do
    resource_1 = %Anoma.Resource{quantity: 1}
    resource_2 = %Anoma.Resource{quantity: 1, prefix: <<131, 109, 255>>}

    tx = PartialTx.empty()

    assert PartialTx.balanced(tx)

    tx = tx |> PartialTx.add_input(resource_1) |> PartialTx.add_input(resource_2)

    assert PartialTx.balanced(tx) == false

    tx = tx |> PartialTx.add_output(resource_1) |> PartialTx.add_output(resource_2)

    assert PartialTx.balanced(tx)
  end
end
