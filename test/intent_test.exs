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

  test "Already added intents do not broadcast" do
    {:ok, supervisor} = Intent.start_link(:intents_signal_new)
    resource_1 = %Resource{quantity: 6}
    Communicator.subscribe(:intents_signal_new_com, self())

    Communicator.new_intent(:intents_signal_new_com, resource_1)
    assert_receive {:"$gen_cast", {:new_intent, ^resource_1}}

    Communicator.new_intent(:intents_signal_new_com, resource_1)
    refute_receive {:"$gen_cast", {:new_intent, ^resource_1}}

    Intent.shutdown(supervisor)
  end

  test "If no intent to remove then not broadcasted" do
    {:ok, supervisor} = Intent.start_link(:intents_signal_rem)
    resource_1 = %Resource{quantity: 6}
    Communicator.subscribe(:intents_signal_rem_com, self())

    Communicator.new_intent(:intents_signal_rem_com, resource_1)
    Communicator.remove_intent(:intents_signal_rem_com, resource_1)
    assert_receive {:"$gen_cast", {:remove_intent, ^resource_1}}

    Communicator.remove_intent(:intents_signal_com, resource_1)
    refute_receive {:"$gen_cast", {:remove_intent, ^resource_1}}

    Intent.shutdown(supervisor)
  end
end
