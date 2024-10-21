defmodule Anoma.Node.Examples.EGRPC do
  @moduledoc """
  I contain examples to test the GRPC endpoint of the node.
  """
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Examples.EIndexer
  alias Anoma.Node.Examples.ENode
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.Intent
  alias Anoma.Protobuf.IntentPool.AddIntent
  alias Anoma.Protobuf.IntentPool.ListIntents
  alias Anoma.Protobuf.Intents

  import ExUnit.Assertions

  require Logger

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a GRPC connection to the node.

    ### Fields
    - `:channel` - The channel for making grpc requests.
    """
    field(:channel, any())
    field(:node, ENode.t())
  end

  @doc """
  Given an enode, I connect to its GRPC endpoint.

  The examples in this file are used to test against the client GRPC endpoint.
  The client needs a node to process incoming requests (except prove), so a node is also required
  to run these examples.
  """
  @spec connect_to_node(ENode.t() | nil) :: %EGRPC{}
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
          %EGRPC{channel: channel, node: enode}

        {:error, reason} ->
          Logger.error("GRPC connection failed: #{inspect(reason)}")
          {:error, reason}
      end

    assert Kernel.match?(%EGRPC{}, result)

    result
  end

  @doc """
  I list the intents over grpc on the client.
  """
  def list_intents(%EGRPC{} = client \\ connect_to_node()) do
    request = %ListIntents.Request{}
    {:ok, reply} = Intents.Stub.list_intents(client.channel, request)

    assert reply.intents == []
  end

  @doc """
  I add an intent to the client.
  """
  def add_intent(%EGRPC{} = client \\ connect_to_node()) do
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
  def list_nullifiers(%EGRPC{} = client \\ connect_to_node()) do
    request = %Nullifiers.Request{}
    {:ok, reply} = Intents.Stub.list_nullifiers(client.channel, request)
    assert reply.nullifiers == []
  end

  @doc """
  I list all nullifiers, but work with a non-empty set.
  """
  def list_nullifiers_non_empty(%EGRPC{} = client \\ connect_to_node()) do
    # run the example to create a new nullifier in the indexer
    EIndexer.indexer_reads_nullifier(client.node)

    # the indexer will return a nullifier now
    request = %Nullifiers.Request{}
    {:ok, reply} = Intents.Stub.list_nullifiers(client.channel, request)

    assert reply.nullifiers == [
             <<78, 70, 95, 89, 177, 105, 28, 103, 230, 24, 4>>
           ]
  end

  @doc """
  I list all unrevealed commits.
  """
  def list_unrevealed_commits(%EGRPC{} = client \\ connect_to_node()) do
    request = %UnrevealedCommits.Request{}

    {:ok, reply} =
      Intents.Stub.list_unrevealed_commits(client.channel, request)

    assert reply.commits == []
  end

  @doc """
  I list all unspent resources.
  """
  def list_unspent_resources(%EGRPC{} = client \\ connect_to_node()) do
    request = %UnspentResources.Request{}

    {:ok, reply} =
      Intents.Stub.list_unspent_resources(client.channel, request)

    assert reply.unspent_resources == []
  end
end
