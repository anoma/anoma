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
  def start_node(node_id: node_id) do
    DynamicSupervisor.start_child(
      Anoma.Node.NodeSupervisor,
      {Anoma.Node.Supervisor, [node_id: node_id]}
    )
  end
end
