defmodule Anoma do
  use Application

  alias Anoma.Configuration
  alias Anoma.Crypto.Id

  @moduledoc """
  Documentation for `Anoma`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Anoma.hello()
      :world

  """
  def hello do
    :world
  end

  def start(_type, _args) do
    node_name = Elixir.Node.self()
    filename = Atom.to_string(node_name) <> ".bin"

    key =
      if File.exists?(filename) do
        File.read!(filename) |> :erlang.binary_to_term()
      else
        key = Id.new_keypair()
        File.write!(filename, :erlang.term_to_binary(key))
        key
      end

    config = %{
      network: %{tcp_port: 1234, host: {0, 0, 0, 0}},
      router_key: key,
      kiddiepool_engine: Id.new_keypair()
    }

    children = [
      {Anoma.Node.Transport2.Router, [config]},
      Anoma.Node.Transport2.Supervisor
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
  end

  @spec start_logic([{:use_rocks, any()}, ...]) ::
          :ignore | {:error, any()} | {:ok, pid()}
  @doc """
  I start the Anoma application.

  Given environment `env` I search for a configuration file
  `anoma_env.toml` in the appropriate configuration direction. If the
  configuration refers to a dumped session, we launch it directly.
  Otherwise we launch it with minimal settings.

  If no configuration was found, I provide basic setup for a new Node and
  start it under supervision.
  """
  def start_logic(use_rocks: rocks_flag) do
    config =
      Configuration.default_configuration_location()
      |> Configuration.read_configuration()

    dump_path = Configuration.locate_dump_file(config)

    IO.inspect(dump_path, label: "dump_path")

    if dump_path do
      Anoma.Dump.launch(dump_path, :anoma, supervisor: [name: Anoma])
    else
      Configuration.launch_min(config,
        use_rocksdb: rocks_flag,
        supervisor: [name: Anoma]
      )
    end
  end
end
