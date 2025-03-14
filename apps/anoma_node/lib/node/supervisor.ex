defmodule Anoma.Node.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node.
  """

  require Logger

  use Supervisor

  alias Anoma.Node.Intents
  alias Anoma.Node.Logging
  alias Anoma.Node.Transaction
  alias Anoma.Node.Transport
  alias Anoma.Node.Config

  @spec child_spec(any()) :: map()
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  @spec start_link(Config.t()) :: any()
  def start_link(config) do
    name = Anoma.Node.Registry.via(config.node_id, __MODULE__)
    Supervisor.start_link(__MODULE__, config, name: name)
  end

  @impl true
  @spec init(Config.t()) :: any()
  def init(config) do
    Logger.info("starting node with #{inspect(config)}")
    Process.set_label(__MODULE__)

    node_id = config.node_id
    grpc_port = config.runtime_system_config.node_grpc_port
    transaction = config.startup_arguments

    IO.inspect transaction
    children = [
      {Transport.Supervisor, node_id: node_id},
      {Transaction.Supervisor, [node_id: node_id] ++ transaction},
      {Intents.Supervisor, node_id: node_id},
      {Logging, node_id: node_id}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
