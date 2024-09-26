defmodule Anoma.Node.Transport.Supervisor do
  @moduledoc """
  I am the transport supervisor.

  My main functionality is to supervise the physical transport connections
  in this node (e.g., TCP connections).
  """
  use Supervisor

  require Logger

  alias Anoma.Node.Transport.Router
  alias Anoma.Node.Transport.Registry

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  @doc """
  I initialize a new transport supervision tree.

  ### Options

  - `:node_id` - The key of the local node.
  """
  def init(node_id: node_id) do
    Logger.debug("starting transport supervisor #{inspect(node_id)}")

    # registry for naming all the processes
    registry_name = Registry.registry_name(node_id)
    registry = {Elixir.Registry, keys: :unique, name: registry_name}

    # router
    router = {Router, [node_id: node_id]}

    # tcp supervisor
    tcp_supervisor =
      {
        DynamicSupervisor,
        name: Registry.key(node_id, node_id, :tcp_supervisor),
        strategy: :one_for_one,
        max_restarts: 1_000_000,
        max_seconds: 1
      }

    # proxy supervisor
    proxy_supervisor =
      {DynamicSupervisor,
       name: Registry.key(node_id, node_id, :proxy_engine_supervisor),
       strategy: :one_for_one,
       max_restarts: 1_000_000,
       max_seconds: 1}

    # start the supervisor
    children = [
      registry,
      router,
      tcp_supervisor,
      proxy_supervisor
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
