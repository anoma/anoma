defmodule Anoma.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node application.

  I manage the shared processes and multiple nodes.

  ### Shared Processes
   - Registry
   - NodeSupervisor
  """

  use Supervisor

  alias Anoma.Node
  alias Anoma.Node.Replay.State
  alias Anoma.Node.Tables

  ############################################################
  #                       Supervisor Implementation          #
  ############################################################

  @spec start_link(any()) :: Supervisor.on_start()
  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    Process.set_label(__MODULE__)

    :ok = Anoma.Node.Tables.initialize_storage()

    children = [
      {Elixir.Registry, keys: :unique, name: Anoma.Node.Registry},
      {DynamicSupervisor, name: Anoma.Node.NodeSupervisor}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  ############################################################
  #                       Public                             #
  ############################################################

  @doc """
  I start a new node with the given `node_id`.
  """
  @spec start_node(Node.Supervisor.args_t()) ::
          DynamicSupervisor.on_start_child()
  def start_node(args) do
    node_id = args[:node_id]

    with {:ok, init_args} <- State.startup_arguments_or_default(node_id),
         {:ok, _} <- initialize_storage(node_id) do
      # put the arguments in the given arguments
      args = Keyword.put_new(args, :transaction, init_args)

      DynamicSupervisor.start_child(
        Anoma.Node.NodeSupervisor,
        {Anoma.Node.Supervisor, args}
      )
    else
      {:error, :failed_to_initialize_storage} ->
        {:error, :failed_to_start_node, :failed_to_initialize_storage}
    end
  end

  @doc """
  Given a node id, I stop that node completely.
  """
  @spec stop_node(String.t()) :: :ok
  def stop_node(node_id) do
    Anoma.Node.Registry.via(node_id, Anoma.Node.Supervisor)

    Supervisor.stop(Anoma.Node.Registry.via(node_id, Anoma.Node.Supervisor))
  end

  ############################################################
  #                  Private Helpers                         #
  ############################################################

  @spec initialize_storage(String.t()) ::
          {:ok, :existing_node | :new_node}
          | {:error, :failed_to_initialize_storage}
  defp initialize_storage(node_id) do
    # check if the node has existing tables, and initialize them if need be.
    case Tables.initialize_tables_for_node(node_id) do
      {:ok, :created} ->
        {:ok, :new_node}

      {:ok, :existing} ->
        {:ok, :existing_node}

      {:error, _e} ->
        {:error, :failed_to_initialize_storage}
    end
  end
end
