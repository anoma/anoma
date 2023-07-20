defmodule AnomaTest.Communicator do
  use ExUnit.Case, async: true

  import Anoma.Communicator

  alias Anoma.Communicator

  alias Anoma.Subscriber.Basic

  doctest(Anoma.Communicator)

  test "subscribers properly get intents messages" do
    {_, comms} = GenServer.start_link(Communicator, [])

    {_, sub} = GenServer.start_link(Basic, init: [], home: self)

    Anoma.Communicator.subscribe(comms, sub)

    resource_1 = %Anoma.Resource{quantity: 1}

    resource_2 = %Anoma.Resource{quantity: 2}

    Communicator.new_intent(comms, resource_1)

    assert_receive :received_intent, 200

    assert Basic.dump_state(sub).intents |> length() == 1

    Communicator.new_intent(comms, resource_2)

    assert_receive :received_intent, 200

    assert Basic.dump_state(sub).intents |> length() == 2
  end
end
