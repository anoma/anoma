defmodule Anoma.Client.Examples.EClient do
  use Anoma.Client.Web.ConnCase

  @moduledoc """
  I contain functions to test the public interface of the client.

  I start a new client and if necessary a node, and then connect to that node.

  I test the public GRPC interface of the client to ensure it works as expected.
  """
  use TypedStruct

  alias Anoma.Client
  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.ENode

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:node`       - The node to which the client is connected.
    - `:client`     - The client that is connected to the node.
    - `:conn`       - A phoenix connection object to make web requests.
    """
    field(:node, ENode.t())
    field(:client, Client.t())
    field(:channel, any())
    field(:conn, any())
  end

  ############################################################
  #                    Helpers                               #
  ############################################################

  @doc """
  I create an instance of the client and connect it to the given node.

  If there is already a client started, I kill it and start a new one.
  """
  @spec create_example_client(ENode.t() | nil) :: EClient.t()
  def create_example_client(enode \\ ENode.start_noded()) do
    # kill previous clients (the client only connects to one node at this time)
    DynamicSupervisor.which_children(Anoma.Client.ConnectionSupervisor)
    |> Enum.map(fn {_, pid, _, _} ->
      DynamicSupervisor.terminate_child(
        Anoma.Client.ConnectionSupervisor,
        pid
      )
    end)

    {:ok, client} =
      Client.connect(
        enode.node_config.instance.node_grpc_host,
        enode.node_config.instance.node_grpc_port,
        enode.node_config.node_id
      )

    %EClient{client: client, node: enode, conn: Phoenix.ConnTest.build_conn()}
  end

  @doc """
  I create the setup necessary to run each example below without arguments.
  """
  @spec setup() :: EClient.t()
  def setup() do
    create_example_client()
  end
end
