defmodule Anoma.Client.Examples.EProxy do
  @moduledoc """
  I contain examples for the GRPC proxy.

  The proxy is started, and if necessary, a node is started too.

  I then test each public API of the proxy to ensure it works as expected.
  """
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Examples.ENode
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Intent

  require ExUnit.Assertions

  import ExUnit.Assertions

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """

    ### Fields
    - `:pid`     - The process id of the GRPC proxy.
    """
    field(:pid, pid())
  end

  @doc """
  I test the GRPC proxy to proxy requests to an existing Anoma node.
  """
  @spec start_proxy_for(ENode.t() | nil) :: %EProxy{} | {:error, term()}
  def start_proxy_for(enode \\ nil) do
    # if no node was given, this ran in a unit test.
    # we kill all nodes since we can only have a local node for this test.
    enode =
      if enode == nil do
        ENode.kill_all_nodes()
        ENode.start_node(grpc_port: 0)
      else
        enode
      end

    proxy_args = [port: enode.grpc_port, host: "localhost"]

    case GRPCProxy.start_link(proxy_args) do
      {:ok, pid} ->
        %EProxy{pid: pid}

      {:error, {:already_started, pid}} ->
        %EProxy{pid: pid}

      err ->
        {:error, err}
    end
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_intents(EProxy.t()) ::
          {:ok, Anoma.Protobuf.IntentPool.ListIntents.Response.t()}
  def list_intents(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_intents()
    assert Kernel.match?({:ok, %{intents: []}}, result)
    result
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec add_intent(EProxy.t()) ::
          {:ok, Anoma.Protobuf.IntentPool.AddIntent.Response.t()}
  def add_intent(%EProxy{} \\ start_proxy_for()) do
    intent = %Intent{value: 1}
    result = GRPCProxy.add_intent(intent)
    assert Kernel.match?({:ok, %{result: "intent added"}}, result)
    result
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_nullifiers(EProxy.t()) ::
          {:ok, Anoma.Protobuf.Indexer.Nullifiers.Response.t()}
  def list_nullifiers(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_nullifiers()
    assert Kernel.match?({:ok, %{nullifiers: []}}, result)
    result
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_unrevealed_commits(EProxy.t()) ::
          {:ok, Anoma.Protobuf.Indexer.UnrevealedCommits.Response.t()}
  def list_unrevealed_commits(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_unrevealed_commits()
    assert Kernel.match?({:ok, %{commits: []}}, result)
    result
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_unspent_resources(EProxy.t()) ::
          {:ok, Anoma.Protobuf.Indexer.UnspentResources.Response.t()}
  def list_unspent_resources(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_unspent_resources()

    assert Kernel.match?(
             {:ok,
              %{
                unspent_resources: []
              }},
             result
           )

    result
  end
end
