defmodule Anoma.Node.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node.
  """

  use Supervisor

  require Logger

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :tx_args])
    node_id = args[:node_id]
    name = Anoma.Node.Registry.name(node_id, __MODULE__)

    Supervisor.start_link(__MODULE__, args, name: name)
  end

  def init(args) do
    Logger.debug("starting node with #{inspect(args)}")

    args = Keyword.validate!(args, [:node_id, :tx_args])
    node_id = args[:node_id]

    children = [
      {Anoma.Node.Transport.Supervisor, node_id: node_id},
      {Anoma.Node.Transaction.Supervisor,
       [node_id: node_id, tx_args: args[:tx_args]]},
      {Anoma.Node.Utility.Supervisor, node_id: node_id},
      {Anoma.Node.Logging, node_id: node_id}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
