defmodule Anoma.Node.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node.
  """

  use Supervisor

  require Logger

  @spec child_spec(any()) :: map()
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  @spec start_link(
          list(
            {:node_id, String.t()}
            | {:grpc_port, non_neg_integer}
            | {:tx_args, any()}
          )
        ) :: term()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :grpc_port, :tx_args])
    name = Anoma.Node.Registry.via(args[:node_id], __MODULE__)
    Supervisor.start_link(__MODULE__, args, name: name)
  end

  @impl true
  def init(args) do
    Logger.debug("starting node with #{inspect(args)}")
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, [:node_id, :tx_args, grpc_port: 0])

    children = [
      {Anoma.Node.Transport.Supervisor,
       node_id: args[:node_id], grpc_port: args[:grpc_port]},
      {Anoma.Node.Transaction.Supervisor,
       [node_id: args[:node_id], tx_args: args[:tx_args]]},
      {Anoma.Node.Intents.Supervisor, node_id: args[:node_id]},
      {Anoma.Node.Logging, node_id: args[:node_id]}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
