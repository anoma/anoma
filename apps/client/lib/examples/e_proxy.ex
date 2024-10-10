defmodule Client.Examples.EProxy do
  @moduledoc """
  I contain examples for the GRPC proxy.

  The proxy is started, and if necessary, a node is started too.

  I then test each public API of the proxy to ensure it works as expected.
  """
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Examples.ENode
  alias Client.Connection.GRPCProxy
  alias Protobufs.Intent

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
  @spec start_proxy_for(ENode.t()) :: %EProxy{} | {:error, term()}
  def start_proxy_for(enode \\ ENode.start_node(grpc_port: 0)) do
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
          {:ok, Protobufs.IntentPool.ListIntents.Response.t()}
  def list_intents(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_intents()
    assert Kernel.match?({:ok, %{intents: []}}, result)
    result
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec add_intent(EProxy.t()) ::
          {:ok, Protobufs.IntentPool.AddIntent.Response.t()}
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
          {:ok, Protobufs.Indexer.Nullifiers.Response.t()}
  def list_nullifiers(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_nullifiers()
    assert Kernel.match?({:ok, %{nullifiers: ["null", "ifier"]}}, result)
    result
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_unrevealed_commits(EProxy.t()) ::
          {:ok, Protobufs.Indexer.UnrevealedCommits.Response.t()}
  def list_unrevealed_commits(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_unrevealed_commits()
    assert Kernel.match?({:ok, %{commits: ["commit1", "commit2"]}}, result)
    result
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_unspent_resources(EProxy.t()) ::
          {:ok, Protobufs.Indexer.UnspentResources.Response.t()}
  def list_unspent_resources(%EProxy{} \\ start_proxy_for()) do
    result = GRPCProxy.list_unspent_resources()

    assert Kernel.match?(
             {:ok,
              %{
                unspent_resources: [
                  "unspent resource 1",
                  "unspent resource 2"
                ]
              }},
             result
           )

    result
  end
end
