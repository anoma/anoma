defmodule AnomaTest.Node.Config do
  use ExUnit.Case, async: true

  alias Anoma.Configuration
  alias Anoma.System.Directories

  test "minimal config launch works" do
    configuration =
      Configuration.configuration(%{"node" => %{"name" => "test_node"}})

    assert {:ok, pid} = Configuration.launch_min(configuration, testing: true)

    Supervisor.stop(pid)
  end

  test "dumped state configuration" do
    config_name = "config_test.toml"

    config_path = config_name |> Directories.configuration()

    config = Configuration.configuration()

    assert Configuration.save(config, config_path) == config_path

    load_config = Configuration.read_configuration(config_path)

    assert load_config == config

    config_2 =
      Configuration.configuration(%{"node" => %{"name" => "test_node"}})

    assert config_2 != config

    assert Configuration.save(config_2, config_path) == config_path

    load_config_2 = Configuration.read_configuration(config_path)

    assert load_config_2 == config_2

    File.rm(config_name)
  end

  # Please add validation testing
end
