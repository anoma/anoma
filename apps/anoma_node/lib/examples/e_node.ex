defmodule Anoma.Node.Examples.ENode do
  use TypedStruct

  import ExUnit.Assertions

  alias __MODULE__
  alias Anoma.Node.Examples.ERegistry
  alias Anoma.Crypto.Id

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:node_id`   - The key of this router. This value is used to announce myself to other
    - `:pid`       - the pid of the supervision tree.
    - `:tcp_ports` - The ports on which the node is listening for connections.
    """
    field(:node_id, Id.t())
    field(:pid, pid())
    field(:tcp_ports, [integer()], default: [])
  end

  ############################################################
  #                  Public API                              #
  ############################################################

  @doc """
  I start a new node given a node id and returns its process id.
  """
  @spec start_node(Id.t()) :: ENode.t()
  def start_node(node_id) do
    pid =
      case Anoma.Supervisor.start_node(node_id: node_id) do
        {:ok, pid} ->
          pid

        {:error, {:already_started, pid}} ->
          pid
      end

    # make some assertions about the running processes for this node
    assert ERegistry.process_registered?(node_id, :tcp_supervisor)
    assert ERegistry.process_registered?(node_id, :proxy_supervisor)

    %ENode{node_id: node_id, pid: pid, tcp_ports: []}
  end

  @doc """
  I stop a node and assert that's is gone.
  """
  @spec stop_node(ENode.t()) :: :ok
  def stop_node(node) do
    Supervisor.stop(node.pid)

    assert ERegistry.process_registered?(node.node_id, :tcp_supervisor)
    assert ERegistry.process_registered?(node.node_id, :proxy_supervisor)
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
