defmodule AnomaTest.Node.Config do
  use ExUnit.Case, async: true

  alias Anoma.Configuration

  test "minimal config launch works" do
    Configuration.create_min(
      "test/node/config_test",
      "config_test",
      "qualified_config_test",
      "order_config_test",
      "snapshot_path_config_test",
      "block_storage_config_test",
      :no_timer
    )

    {:ok, _pid} = Anoma.Configuration.launch_min("test/node/config_test.toml")

    Configuration.remove_config("test/node/config_test.toml")
  end
end
