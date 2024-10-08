defmodule Anoma.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node application.

  I manage the shared processes and multiple nodes.

  ### Shared Processes
   - Registry
   - NodeSupervisor
  """

  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_args) do
    children = [
      {Elixir.Registry, keys: :unique, name: Anoma.Node.Registry},
      {DynamicSupervisor, name: Anoma.Node.NodeSupervisor}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  @doc """
  I start a new node with the given `node_id` and transaction initialization arguments.
  """
  def start_node(args) do
    [tx_args: tx_args, node_id: node_id] =
      Keyword.validate!(args, [
        :node_id,
        tx_args: [mempool: [], ordering: [], storage: []]
      ])

    DynamicSupervisor.start_child(
      Anoma.Node.NodeSupervisor,
      {Anoma.Node.Supervisor, [node_id: node_id, tx_args: tx_args]}
    )
  end
end
