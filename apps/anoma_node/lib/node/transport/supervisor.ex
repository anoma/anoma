defmodule Anoma.Node.Transport.Supervisor do
  @moduledoc """
  I am the transport supervisor.

  My main functionality is to supervise the physical transport connections
  in this node (e.g., TCP connections).
  """
  use Supervisor

  require Logger

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.NetworkRegister
  alias Anoma.Node.Transport

  @spec start_link([any()]) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :node_config])
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  @doc """
  I initialize a new transport supervision tree.

  ### Options

  - `:node_id` - The key of the local node.
  """
  def init(args) do
    Logger.debug("starting transport supervisor #{inspect(args)}")
    Process.set_label(__MODULE__)

    # validate args and set defaults
    args = Keyword.validate!(args, [:node_id, :node_config])
    node_id = args[:node_id]

    children = [
      {DynamicSupervisor,
       name: Registry.via(node_id, Transport.ProxySupervisor),
       strategy: :one_for_one,
       max_restarts: 1_000_000,
       max_seconds: 1},
      {NetworkRegister, [node_id: node_id, node_config: args[:node_config]]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
