defmodule Anoma.Configuration do
  @moduledoc """
  I am a configuration module. I read the provided TOML configuration file
  and feed the apporpriate info for Node launching

  The codebase has a corresponding file that can inform the user of the
  format I expect.

  ### Public API

  - `create_min/0`
  - `create_min/7`
  - `launch_min/1`
  - `parse_node/1`
  - `parse/1`
  """

  alias Anoma.System.Directories

  @doc """
  I create a minimal configuration file on application startup unless the
  environment is :test

  The configuration file is created with the [node] header and with keys:

  name = 'name'
  qualified = 'qualified_name'
  order = 'order_name'
  snapshot_path = 'path'
  block_storage = 'block'
  ping_time = 10000

  Check `parse_min/1` to see how these will be made into node parameters

  The file is wirtten to the XDG-specified config folder unless the
  environment is :test
  """
  def create_min() do
    unless Application.get_env(:anoma, :env) == :test do
      path = Directories.configuration("/config.toml")

      path |> File.write!("[node] \n
        name = 'name' \n
        qualified = 'qualified_name' \n
        order = 'order_name' \n
        snapshot_path = 'path' \n
        block_storage = 'block' \n
        ping_time = 10000")
    end
  end

  @doc """
  I create a minimal configuration file on application startup

  Given arguments config_name, name, qualified, order, snap, block, timer
  in that order, the configuration file is created with the [node] header
  and with keys:

  name = 'name'
  qualified = 'qualified'
  order = 'order'
  snapshot_path = 'path'
  block_storage = 'block'
  ping_time = maybe_time

  where maybe_timer is time if the latter is an integer and 'no_timer'
  if time == :no_timer

  Check `parse_min/1` to see how these will be made into node parameters

  The file is wirtten to the XDG-specified config folder. If the
  environment is :test, the file is written in immediate folder with name
  config_name
  """
  @spec create_min(
          String.t(),
          String.t(),
          String.t(),
          String.t(),
          String.t(),
          String.t(),
          :no_timer | integer
        ) :: :ok
  def create_min(config_name, name, qualified, order, snap, block, timer) do
    file_name = "/#{config_name}.toml"

    path = Directories.configuration(file_name)

    # Ensure the directory exists
    File.mkdir_p!(Path.dirname(path))

    path |> File.write!("[node] \n
      name = '#{name}' \n
      qualified = '#{qualified}' \n
      order = '#{order}' \n
      snapshot_path = '#{snap}' \n
      block_storage = '#{block}' \n
      #{time_parse(timer)}")
  end

  @doc """
  Given a file path to a TOML file with minimal node startup I launch the
  node with the appopriate name
  """
  def launch_min(file_path) do
    path = Directories.configuration(file_path)
    map = parse(path)
    settings = map |> parse_min() |> tl() |> Anoma.Node.start_min()

    Anoma.Node.start_link(
      new_storage: true,
      name: map[:name],
      use_rocks: false,
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
       %Anoma.Node.Storage{
         qualified: node["qualified"] |> String.to_atom(),
         order: node["order"] |> String.to_atom()
       }},
      {:block_storage, node["block_storage"] |> String.to_atom()},
      {:ping_time, node["ping_time"] |> maybe_ping()}
    ]
  end

  @spec remove_config(String.t()) :: :ok
  @spec remove_config(String.t(), atom()) :: :ok
  def remove_config(file_path, env \\ Application.get_env(:anoma, :env)) do
    file_path
    |> Directories.configuration(env)
    |> File.rm!()
  end

  ############################################################
  #                         Helpers                          #
  ############################################################

  @doc """
  I decode the TOML file provided a path to it, ignoring if
  unsuccesful
  """
  def parse(name) do
    with {:ok, map} <- Toml.decode_file(name) do
      map
    end
  end

  defp time_parse(time) do
    case time do
      time when is_integer(time) -> "ping_time = #{time}"
      :no_timer -> "ping_time = 'no_timer'"
      _ -> :error
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
