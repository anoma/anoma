defmodule Anoma.Client.Node.Connection.Supervisor do
  @moduledoc """
  I am the client supervisor. I monitor all the processed regarding a connection
  to a single remote node.

  I manage two connections. The GRPC endpoint for this client and the proxy for
  the remote node.
  """
  use Supervisor

  require Logger

  alias Anoma.Client.Node.GRPCProxy

  @args [
    :node_id,
    :host,
    :port,
    :grpc_port,
    :client_id
  ]

  ############################################################
  #                       Types                              #
  ############################################################

  @typep startup_options() :: [
           {:node_id, String.t()},
           {:host, String.t()},
           {:port, integer()},
           {:grpc_port, integer()},
           {:client_id, String.t()}
         ]

  ############################################################
  #                       State                              #
  ############################################################

  @doc """
  Returns the child specification for a connection to the remote node.

  These connections must be terminated when the client is stopped. This is why
  the restart is set to `:transient`.
  """
  @spec child_spec(startup_options) :: Supervisor.child_spec()
  def child_spec(args) do
    %{
      id: __MODULE__,
      restart: :transient,
      start: {__MODULE__, :start_link, [args]}
    }
  end

  @doc """
  I start_link a new client connection supervision tree.
  """
  @spec start_link(startup_options) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, @args)
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  @doc """
  I initialize a new client connection supervision tree.
  """
  def init(args) do
    Logger.debug("starting client supervisor #{inspect(args)}")
    args = Keyword.validate!(args, @args)
    children = [{GRPCProxy, args}]
    Supervisor.init(children, strategy: :one_for_one)
  end
end
