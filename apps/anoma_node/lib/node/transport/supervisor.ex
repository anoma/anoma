defmodule Anoma.Node.Transport.Supervisor do
  @moduledoc """
  I am the transport supervisor.

  My main functionality is to supervise the physical transport connections
  in this node (e.g., TCP connections).
  """
  use Supervisor

  require Logger

  @spec start_link([any()]) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, grpc_port: 0])
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
    args = Keyword.validate!(args, [:node_id, grpc_port: 0])

    # start the supervisor
    children = [
      {GRPC.Server.Supervisor,
       endpoint: Anoma.Node.Transport.GRPC.Endpoint,
       port: args[:grpc_port],
       start_server: true}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
