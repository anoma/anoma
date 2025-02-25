defmodule Anoma.Client.Connection.Supervisor do
  @moduledoc """
  I am the client supervisor. I monitor all the processed regarding a connection to a single remote node.

  I manage two connections. The GRPC endpoint for this client and the proxy for the remote node.
  """

  require Logger

  use Supervisor

  @spec child_spec([any()]) :: Supervisor.child_spec()
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

  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(args) do
    args =
      Keyword.validate!(args, [
        :node_id,
        :listen_port,
        :host,
        :port,
        type: :grpc
      ])

    if args[:type] != :grpc do
      raise ArgumentError, "only grpc connections are supported at the moment"
    end

    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  @doc """
  I initialize a new client connection supervision tree.
  """
  def init(args) do
    Logger.debug("starting client supervisor #{inspect(args)}")

    args =
      Keyword.validate!(args, [
        :node_id,
        :listen_port,
        :host,
        :port,
        type: :grpc
      ])

    children = [
      {Anoma.Client.Connection.GRPCProxy,
       Keyword.take(args, [:host, :port, :node_id])},
      GrpcReflection,
      {GRPC.Server.Supervisor,
       endpoint: Anoma.Client.Api.Endpoint,
       port: args[:listen_port],
       start_server: true}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
