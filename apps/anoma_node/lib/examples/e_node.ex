defmodule Anoma.Node.Examples.ENode do
  use TypedStruct

  alias __MODULE__

  require Logger

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
    """
    field(:node_id, String.t())
    field(:pid, pid())
  end

  ############################################################
  #                  Public API                              #
  ############################################################

  @doc """
  I start a new node given a node id and returns its process id.

  When a node is started, I put its ENode struct in an ETS table for later retrieval.

  When a node is already spawned, I lookup the ENode struct in the ETS table.
  Some meta data (in particular, the GRPC port) is only available when the node is started
  so I fetch that data from the ETS table.
  """
  @spec start_node(Keyword.t()) :: ENode.t() | {:error, :failed_to_start_node}
  def start_node(opts \\ []) do
    node_id = "#{:erlang.phash2(make_ref())}"
    grpc_port = Application.get_env(:anoma_node, :grpc_port)

    opts =
      Keyword.validate!(opts,
        node_config: %{
          node_id: node_id,
          grpc_host: "localhost",
          grpc_port: grpc_port
        },
        node_id: node_id
      )

    enode =
      case Anoma.Supervisor.start_node(opts) do
        {:ok, pid} ->
          %ENode{
            node_id: opts[:node_id],
            pid: pid
          }

        {:error, {:already_started, pid}} ->
          %ENode{
            node_id: opts[:node_id],
            pid: pid
          }

        {:error, _} ->
          {:error, :failed_to_start_node}
      end

    case enode do
      {:error, _} ->
        enode

      enode ->
        enode
    end
  end

  @doc """
  I stop a node and assert that's is gone.
  """
  @spec stop_node(ENode.t()) :: :ok
  def stop_node(node) do
    Supervisor.stop(node.pid)

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
