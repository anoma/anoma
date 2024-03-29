defmodule AnomaTest.Node.Config do
  use ExUnit.Case, async: true

  test "minimal config launch works" do
    {:ok, _pid} = Anoma.Configuration.launch_min("test/node/config_test.toml")
  end
end
