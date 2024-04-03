defmodule Anoma.Configuration do
  @moduledoc """
  I am a configuration module. I read the provided TOML configuration file
  and feed the apporpriate info for Node launching

  The codebase has a corresponding file that can inform the user of the
  format I expect.

  ### Public API

  - `launch_min/1`
  - `parse_node/1`
  - `parse/1`
  """

  @doc """
  Given a file path to a TOML file with minimal node startup I launch the
  node with the appopriate name
  """
  def launch_min(path) do
    map = parse(path)

    settings = map |> parse_min() |> tl() |> Anoma.Node.start_min()

    Anoma.Node.start_link(
      new_storage: true,
      name: map[:name],
      settings: settings
    )
  end

  @doc """
  Given a map, I decode all the needed info for a minimal node startup
  and put it in the appropriate keyword list
  """

  def parse_min(map) do
    node = map["node"]
    path = node["snapshot_path"] |> String.to_atom()

    [
      {:name, node["name"] |> String.to_atom()},
      {:snapshot_path, [path | 0]},
      {:storage_data,
       %Anoma.Storage{
         qualified: node["qualified"] |> String.to_atom(),
         order: node["order"] |> String.to_atom()
       }},
      {:block_storage, node["block_storage"] |> String.to_atom()},
      {:ping_time, node["ping_time"] |> maybe_ping()}
    ]
  end

  @doc """
  I decode the TOML file provided a path to it, ignoring if
  unsuccesful
  """
  def parse(name) do
    with {:ok, map} <- Toml.decode_file(name) do
      map
    end
  end

  defp maybe_ping(ping) do
    if is_integer(ping) do
      ping
    else
      ping |> String.to_atom()
    end
  end
end
