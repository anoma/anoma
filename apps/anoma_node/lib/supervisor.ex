defmodule Anoma.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node application.

  I manage the shared processes and multiple nodes.

  ### Shared Processes
   - Registry
   - NodeSupervisor
  """

  use Supervisor

  @spec start_link(any()) :: Supervisor.on_start()
  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    Process.set_label(__MODULE__)

    children = [
      {Elixir.Registry, keys: :unique, name: Anoma.Node.Registry},
      {DynamicSupervisor, name: Anoma.Node.NodeSupervisor}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  @doc """
  I start a new node with the given `node_id`.
  """
  @spec start_node(
          list(
            {:node_id, String.t()}
            | {:grpc_port, non_neg_integer()}
            | {:tx_args, any()}
          )
        ) :: DynamicSupervisor.on_start_child()
  def start_node(args) do
    args =
      Keyword.validate!(args, [
        :node_id,
        :grpc_port,
        tx_args: [mempool: [], ordering: [], storage: []]
      ])

    DynamicSupervisor.start_child(
      Anoma.Node.NodeSupervisor,
      {Anoma.Node.Supervisor, args}
    )
  end
end
