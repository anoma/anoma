defmodule Anoma.Node.Examples.EGRPC do
  @moduledoc """
  I contain examples to test the GRPC endpoint of the node.
  """
  use TypedStruct

  alias Anoma.Node.Examples.EGRPC
  alias Anoma.Node.Examples.ENode
  alias Anoma.Proto.Intentpool.Add
  alias Anoma.Proto.Intentpool.Intent
  alias Anoma.Proto.Intentpool.List
  alias Anoma.Proto.IntentpoolService
  alias Anoma.Proto.Node
  alias Examples.ETransparent.ETransaction

  import ExUnit.Assertions
  import ExUnit.CaptureLog

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
    grpc_port = Application.get_env(:anoma_node, :grpc_port)

    enode =
      if enode == nil do
        ENode.kill_all_nodes()
        ENode.start_noded()
      else
        enode
      end

    result =
      case GRPC.Stub.connect("localhost:#{grpc_port}") do
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
    node = %Node{id: client.node.node_config.node_id}
    request = %List.Request{node: node}

    {:ok, reply} = IntentpoolService.Stub.list(client.channel, request)

    assert reply.intents == []
  end

  @doc """
  I list the intents over grpc on the client and expect a failure by not
  providing a node id.
  """
  @spec list_intents_fail(EGRPC.t()) :: boolean()
  def list_intents_fail(%EGRPC{} = client \\ connect_to_node()) do
    request = %List.Request{}

    expected =
      {:error, %GRPC.RPCError{status: 3, message: "node can not be nil"}}

    assert capture_log(fn ->
             assert expected ==
                      IntentpoolService.Stub.list(
                        client.channel,
                        request
                      )

             # this process sleep is here to ensure that the logs are printed and caught by capture_log.
             # I don't know how to fix it otherwise. We can duel over it, Jeremy.
             Process.sleep(1000)
           end) =~ "node can not be nil"
  end

  @doc """
  I list the intents over grpc on the client, but I provide a wrong node id.
  """
  @spec list_intents_invalid_node(EGRPC.t()) :: boolean()
  def list_intents_invalid_node(%EGRPC{} = client \\ connect_to_node()) do
    # we assume here that nodeid deadbeef does not exist.
    node = %Node{id: "wrong"}
    request = %List.Request{node: node}

    expected =
      {:error,
       %GRPC.RPCError{
         __exception__: true,
         message: "node id does not exist",
         status: 3
       }}

    assert capture_log(fn ->
             assert expected ==
                      IntentpoolService.Stub.list(
                        client.channel,
                        request
                      )

             # this process sleep is here to ensure that the logs are printed and caught by capture_log.
             # I don't know how to fix it otherwise. We can duel over it, Jeremy.
             Process.sleep(1000)
           end) =~ "node id does not exist"
  end

  @doc """
  I add an intent to the client.
  """
  @spec add_intent(EGRPC.t()) :: boolean()
  def add_intent(%EGRPC{} = client \\ connect_to_node()) do
    node_id = %Node{id: client.node.node_config.node_id}

    # create an arbitrary intent and jam it
    intent_jammed =
      ETransaction.nullify_intent()
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    request = %Add.Request{
      node: node_id,
      intent: %Intent{intent: intent_jammed}
    }

    {:ok, _reply} = IntentpoolService.Stub.add(client.channel, request)

    # fetch the intents to ensure it was added
    request = %List.Request{node: node_id}

    {:ok, reply} = IntentpoolService.Stub.list(client.channel, request)

    intents = Enum.map(reply.intents, &Map.get(&1, :intent))

    assert intents == [intent_jammed]
  end

  @doc """
  I make a request to add an intent, but without an intent.

  I expect an error to occur.
  """
  def add_intent_fail_no_intent(%EGRPC{} = client \\ connect_to_node()) do
    node = %Node{id: client.node.node_config.node_id}
    request = %Add.Request{node: node}

    assert capture_log(fn ->
             result = IntentpoolService.Stub.add(client.channel, request)

             assert result ==
                      {:error,
                       %GRPC.RPCError{
                         status: 2,
                         message: "intent can not be nil"
                       }}

             Process.sleep(1000)
           end) =~ "intent can not be nil"
  end
end
