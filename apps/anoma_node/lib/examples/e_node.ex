defmodule Anoma.Node.Examples.ENode do
  alias __MODULE__
  alias Anoma.Node.Examples.ERegistry

  require Logger

  import ExUnit.Assertions

  use TypedStruct

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:node_id`    - The key of this router. This value is used to announce myself to other
    - `:pid`        - the pid of the supervision tree.
    - `:tcp_ports`  - The ports on which the node is listening for connections.
    - `:grpc_ports` - The ports on which the node is listening for grpc connections.
    """
    field(:node_id, String.t())
    field(:pid, pid())
    field(:tcp_ports, [integer()], default: [])
    field(:grpc_port, integer(), default: 0)
  end

  ############################################################
  #                  Public API                              #
  ############################################################

  @doc """
  I return a random node id.
  """
  @spec random_node_id() :: String.t()
  def random_node_id() do
    Base.encode64(:crypto.strong_rand_bytes(8))
  end

  @table_name :enode_table

  @doc """
  I initialize the ETS table to keep track of creates nodes.

  My only reason for existence is to keep track of the nodes that are created in the system and their
  GRPC ports.
  """
  @spec initialize_ets() :: atom()
  def initialize_ets() do
    unless @table_name in :ets.all() do
      :ets.new(@table_name, [:named_table, :set, :named_table, :public])
    end

    @table_name
  end

  @doc """
  I start a new node given a node id and returns its process id.

  When a node is started, I put its ENode struct in an ETS table for later retrieval.

  When a node is already spawned, I lookup the ENode struct in the ETS table.
  Some meta data (in particular, the GRPC port) is only available when the node is started
  so I fetch that data from the ETS table.
  """
  @spec start_node(Keyword.t()) :: ENode.t() | {:error, :failed_to_start_node}
  def start_node(opts \\ []) do
    initialize_ets()

    opts =
      Keyword.validate!(opts,
        node_id: "#{:erlang.phash2(make_ref())}",
        grpc_port: 0
      )

    enode =
      case Anoma.Supervisor.start_node(opts) do
        {:ok, pid} ->
          enode = %ENode{
            node_id: opts[:node_id],
            pid: pid,
            tcp_ports: [],
            grpc_port:
              :ranch.get_port(<<"Anoma.Node.Transport.GRPC.Endpoint">>)
          }

          :ets.insert(@table_name, {pid, enode})

          enode

        {:error, {:already_started, pid}} ->
          case :ets.lookup(@table_name, pid) do
            [{_, enode}] ->
              enode

            _ ->
              {:error, :node_started_but_not_in_cache}
              enode = %ENode{node_id: opts[:node_id], pid: pid, tcp_ports: []}
              :ets.insert(@table_name, {pid, enode})
              enode
          end

        {:error, _} ->
          {:error, :failed_to_start_node}
      end

    case enode do
      {:error, _} ->
        enode

      enode ->
        assert ERegistry.process_registered?(enode.node_id, :tcp_supervisor)
        assert ERegistry.process_registered?(enode.node_id, :proxy_supervisor)
        enode
    end
  end

  @doc """
  I stop a node and assert that's is gone.
  """
  @spec stop_node(ENode.t()) :: :ok
  def stop_node(node) do
    Supervisor.stop(node.pid)

    refute ERegistry.process_registered?(node.node_id, :tcp_supervisor)
    refute ERegistry.process_registered?(node.node_id, :proxy_supervisor)
    :ok
  end

  @doc """
  I kill all the nodes in the vm.
  """
  @spec kill_all_nodes() :: :ok
  def kill_all_nodes() do
    Anoma.Node.NodeSupervisor
    |> DynamicSupervisor.which_children()
    |> Enum.each(fn {_, pid, _, _} -> Supervisor.stop(pid) end)
  end
end
