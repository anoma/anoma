defmodule Anoma.Node.Examples.EGRPC do
  @moduledoc """
  I contain examples to test the GRPC endpoint of the node.
  """
  use TypedStruct

  alias Anoma.Node.Examples.EGRPC
  alias Anoma.Node.Examples.ENode
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.IndexerService
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.Intent
  alias Anoma.Protobuf.Intents.List
  alias Anoma.Protobuf.IntentsService
  alias Anoma.Protobuf.MempoolService
  alias Anoma.Protobuf.NodeInfo
  alias Examples.ETransparent.ETransaction
  alias Anoma.Protobuf.Mempool.AddTransaction

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
    - `:node`    - The node to which the client is connected.
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
  @spec connect_to_node(ENode.t() | nil) :: EGRPC.t()
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
  @spec list_intents(EGRPC.t()) :: boolean()
  def list_intents(%EGRPC{} = client \\ connect_to_node()) do
    node_id = %NodeInfo{node_id: client.node.node_id}
    request = %List.Request{node_info: node_id}

    {:ok, reply} = IntentsService.Stub.list_intents(client.channel, request)

    assert reply.intents == []
  end

  @doc """
  I add an intent to the client.
  """
  @spec add_intent(EGRPC.t()) :: boolean()
  def add_intent(%EGRPC{} = client \\ connect_to_node()) do
    node_id = %NodeInfo{node_id: client.node.node_id}

    # create an arbitrary intent and jam it
    intent_jammed =
      ETransaction.nullify_intent()
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    request = %Add.Request{
      node_info: node_id,
      intent: %Intent{intent: intent_jammed}
    }

    {:ok, _reply} = IntentsService.Stub.add_intent(client.channel, request)

    # fetch the intents to ensure it was added
    request = %List.Request{node_info: node_id}

    {:ok, reply} = IntentsService.Stub.list_intents(client.channel, request)

    assert reply.intents == [intent_jammed]
  end

  @doc """
  I list all nullifiers.
  """
  @spec list_nullifiers(EGRPC.t()) :: term()
  def list_nullifiers(%EGRPC{} = client \\ connect_to_node()) do
    node_id = %NodeInfo{node_id: client.node.node_id}

    request = %Nullifiers.Request{node_info: node_id}

    {:ok, _reply} =
      IndexerService.Stub.list_nullifiers(client.channel, request)
  end

  @doc """
  I list all unrevealed commits.
  """
  @spec list_unrevealed_commits(EGRPC.t()) :: term()
  def list_unrevealed_commits(%EGRPC{} = client \\ connect_to_node()) do
    node_id = %NodeInfo{node_id: client.node.node_id}
    request = %UnrevealedCommits.Request{node_info: node_id}

    {:ok, _reply} =
      IndexerService.Stub.list_unrevealed_commits(client.channel, request)
  end

  @doc """
  I list all unspent resources.
  """
  @spec list_unspent_resources(EGRPC.t()) :: term()
  def list_unspent_resources(%EGRPC{} = client \\ connect_to_node()) do
    node_id = %NodeInfo{node_id: client.node.node_id}
    request = %UnspentResources.Request{node_info: node_id}

    {:ok, _reply} =
      IndexerService.Stub.list_unspent_resources(client.channel, request)
  end

  @doc """
  I add a transaction to the client.
  """
  @spec add_transaction(EGRPC.t()) :: EGRPC.t()
  def add_transaction(%EGRPC{} = client \\ connect_to_node()) do
    node_id = %NodeInfo{node_id: client.node.node_id}

    # create an arbitrary intent and jam it
    intent_jammed =
      ETransaction.nullify_intent()
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    request = %AddTransaction.Request{
      node_info: node_id,
      transaction: intent_jammed
    }

    {:ok, %AddTransaction.Response{}} =
      MempoolService.Stub.add(client.channel, request)

    client
  end
end
