defmodule AnomaTest.Intent do
  use ExUnit.Case, async: true

  alias Anoma.Node.Intent
  alias Anoma.Node.Intent.Communicator
  alias Anoma.Resource

  doctest(Anoma.Node.Intent)

  test "Intents don't double add" do
    {:ok, supervisor} = Intent.start_link(:intents_add)

    resource = %Resource{quantity: 5}
    resource_set = MapSet.new([resource])

    Communicator.new_intent(:intents_add_com, resource)
    Communicator.subscribe(:intents_add_com, self())
    assert_receive {:"$gen_cast", {:intents, ^resource_set}}

    Communicator.new_intent(:intents_add_com, resource)

    assert Communicator.all_intents(:intents_add_com) == resource_set

    # Odd this isn't needed?
    Intent.shutdown(supervisor)
  end

  test "intents remove properly" do
    {:ok, supervisor} = Intent.start_link(:intents_rem)

    resource = %Resource{quantity: 5}
    Communicator.new_intent(:intents_rem_com, resource)
    Communicator.remove_intent(:intents_rem_com, resource)

    assert Communicator.all_intents(:intents_rem_com) == MapSet.new()

    Intent.shutdown(supervisor)
  end
end
