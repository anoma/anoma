defmodule Client.Connection.Supervisor do
  @moduledoc """
  I am the client supervisor. I monitor all the processed regarding a connection to a single remote node.
  """
  use Supervisor

  require Logger

  def child_spec(args) do
    %{
      id: __MODULE__,
      restart: :transient,
      start: {__MODULE__, :start_link, [args]}
    }
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:listen_port, :host, :port, type: :grpc])

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
    Logger.error("starting client supervisor #{inspect(args)}")

    args = Keyword.validate!(args, [:listen_port, :host, :port, type: :grpc])

    children = [
      {Client.Connection.GRPCProxy, Keyword.take(args, [:host, :port])}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
