defmodule AnomaTest.Node do
  use ExUnit.Case, async: true

  alias Anoma.Node.{Communicator, Primary}
  alias Anoma.Node

  doctest(Anoma.Node)

  test "node works" do
    # Node.start_link(:anoma)
    # resource_1 = %Anoma.Resource{quantity: 1}
    # Communicator.new_intent(:anoma_com, resource_1)
    # Primary.dump_state(:anoma)
  end
end
