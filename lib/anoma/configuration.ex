defmodule Anoma.Configuration do
  @moduledoc """

  I am a configuration module. I read the provided TOML configuration file and launch an appropriate
  Anoma instance with given settings.

  I expect the config file to be inside the config folder in the Anoma application.

  """

  def launch(path) do
    map = parse(path)

    map |> parse_node() |> Anoma.Node.start_link()

    map["intent"]["name"]
    |> String.to_atom()
    |> Anoma.Node.Intent.start_link()

    :ok
  end

  def parse_node(map) do
    node = map["node"]
    path = node["snapshot_path"] |> String.to_atom()

    [
      {:name, node["name"] |> String.to_atom()},
      {:snapshot_path, [path | 0]},
      {:storage,
       %Anoma.Storage{
         qualified: node["qualified"] |> String.to_atom(),
         order: node["order"] |> String.to_atom()
       }},
      {:block_storage, node["block_storage"] |> String.to_atom()},
      {:ping_time, node["ping_time"] |> maybe_ping()}
    ]
  end

  def maybe_ping(ping) do
    if is_integer(ping) do
      ping
    else
      ping |> String.to_atom()
    end
  end

  def parse(name) do
    {:ok, map} = Toml.decode_file(name)

    map
  end
end
