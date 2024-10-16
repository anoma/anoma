defmodule Anoma.Client.Examples.EClient do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I start a new client and if necessary a node, and then connect to that node.

  I test the public GRPC interface of the client to ensure it works as expected.
  """
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.ENode
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.Intent
  alias Anoma.Protobuf.IntentPool.AddIntent
  alias Anoma.Protobuf.IntentPool.ListIntents
  alias Anoma.Protobuf.Intents

  import ExUnit.Assertions

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:channel` - The channel for making grpc requests.
    """
    field(:channel, any())
  end

  @doc """
  Given an enode, I connect to its GRPC endpoint.
  """
  @spec connect_to_node(ENode.t() | nil) :: %EClient{}
  def connect_to_node(enode \\ nil) do
    # if no node was given, this ran in a unit test.
    # we kill all nodes since we can only have a local node for this test.
    enode =
      if enode == nil do
        ENode.kill_all_nodes()
        ENode.start_node(grpc_port: 0)
      else
        enode
      end

    result =
      case GRPC.Stub.connect("localhost:#{enode.grpc_port}") do
        {:ok, channel} ->
          %EClient{channel: channel}

        {:error, reason} ->
          {:error, reason}
      end

    assert Kernel.match?(%EClient{}, result)

    result
  end

  @doc """
  I list the intents over grpc on the client.
  """
  def list_intents(%EClient{} = client \\ connect_to_node()) do
    request = %ListIntents.Request{}
    {:ok, _reply} = Intents.Stub.list_intents(client.channel, request)
  end

  @doc """
  I add an intent to the client.
  """
  def add_intent(%EClient{} = client \\ connect_to_node()) do
    request = %AddIntent.Request{
      intent: %Intent{value: 1}
    }

    {:ok, _reply} = Intents.Stub.add_intent(client.channel, request)

    # fetch the intents to ensure it was added
    request = %ListIntents.Request{}

    {:ok, reply} = Intents.Stub.list_intents(client.channel, request)

    assert reply.intents == ["1"]
  end

  @doc """
  I list all nullifiers.
  """
  def list_nullifiers(%EClient{} = client \\ connect_to_node()) do
    request = %Nullifiers.Request{}
    {:ok, _reply} = Intents.Stub.list_nullifiers(client.channel, request)
  end

  @doc """
  I list all unrevealed commits.
  """
  def list_unrevealed_commits(%EClient{} = client \\ connect_to_node()) do
    request = %UnrevealedCommits.Request{}

    {:ok, _reply} =
      Intents.Stub.list_unrevealed_commits(client.channel, request)
  end

  @doc """
  I list all unspent resources.
  """
  def list_unspent_resources(%EClient{} = client \\ connect_to_node()) do
    request = %UnspentResources.Request{}

    {:ok, _reply} =
      Intents.Stub.list_unspent_resources(client.channel, request)
  end

  # commented out until the test is fixed (done in other branch)
  # @doc """
  # I prove something using the client.
  # """
  # def prove_something(%EClient{} = client \\ connect_to_node()) do
  #   request = %Prove.Request{intent: "prove this, please"}
  #   {:ok, _reply} = Intents.Stub.prove(client.channel, request)
  # end
end
