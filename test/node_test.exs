defmodule AnomaTest.Node do
  use TestHelper.TestMacro, async: true

  doctest(Anoma.Node.Executor)

  test "node works" do
    # Node.start_link(:anoma)
    # resource_1 = %Anoma.RM.Resource{quantity: 1}
    # Communicator.new_intent(:anoma_com, resource_1)
    # Primary.dump_state(:anoma)
  end
end
