defmodule AnomaTest.PartialTx do
  use ExUnit.Case, async: true

  alias Anoma.PartialTx

  doctest(Anoma.PartialTx)

  test "indeed we are balanced! Or not!?!?!" do
    resource_1 = %Anoma.Resource{quantity: 1}
    resource_2 = %Anoma.Resource{quantity: 1, prefix: <<131, 109, 255>>}

    tx = PartialTx.empty()

    assert PartialTx.balanced(tx)

    tx =
      tx |> PartialTx.add_input(resource_1) |> PartialTx.add_input(resource_2)

    assert PartialTx.balanced(tx) == false

    tx =
      tx
      |> PartialTx.add_output(resource_1)
      |> PartialTx.add_output(resource_2)

    assert PartialTx.balanced(tx)
  end

  test "double resource add" do
    resource_1 = %Anoma.Resource{quantity: 1}

    tx = PartialTx.empty()

    assert PartialTx.balanced(tx)

    tx =
      tx
      |> PartialTx.add_input(resource_1)
      |> PartialTx.add_output(resource_1)

    assert PartialTx.balanced(tx)

    tx = tx |> PartialTx.add_input(resource_1)

    assert PartialTx.balanced(tx) == false

    tx = tx |> PartialTx.add_output(resource_1)

    assert PartialTx.balanced(tx)
  end

  test "checking validity" do
    r_true = %Anoma.Resource{quantity: 1, logic: 0}
    r_false = %Anoma.Resource{quantity: 1, logic: 1}
    empty = PartialTx.empty()

    assert PartialTx.is_valid(empty)
    assert PartialTx.is_valid(empty |> PartialTx.add_input(r_true))
    assert PartialTx.is_valid(empty |> PartialTx.add_input(r_false)) == false

    multi =
      empty
      |> PartialTx.add_input(r_true)
      |> PartialTx.add_input(r_false)

    assert PartialTx.is_valid(multi) == false
  end
end
