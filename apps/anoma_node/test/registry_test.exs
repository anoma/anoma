defmodule Examples.RegistryTest do
  use TestHelper.TestMacro

  alias Anoma.Node.Examples.ERegistry

  test "registry tests" do
    ERegistry.create_address()
    ERegistry.create_address_with_label()
    ERegistry.generate_name()
    ERegistry.generate_name_with_label()
    ERegistry.find_pid_of_process()
    ERegistry.list_engines_for_node()
  end
end
