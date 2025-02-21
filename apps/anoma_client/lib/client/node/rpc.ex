defmodule Anoma.Client.Node.RPC do
  @moduledoc """
  I contain functions that make requests to the GRPC endpoint of a node.
  """

  alias Anoma.Proto.Intentpool
  alias Anoma.Proto.Intentpool.Intent
  alias Anoma.Proto.IntentpoolService
  alias Anoma.Proto.Mempool
  alias Anoma.Proto.Mempool.Transaction
  alias Anoma.Proto.MempoolService
  alias Anoma.Proto.Node
  alias Anoma.Proto.PubSub.Subscribe
  alias Anoma.Proto.PubSub.Topic
  alias Anoma.Proto.PubSubService
  alias Anoma.Proto.Advertisement.Advertise
  alias Anoma.Proto.Advertisement.GRPCAddress
  alias Anoma.Proto.AdvertisementService

  @spec advertise(GRPC.Channel.t(), any(), any(), any(), any()) ::
          {:error, :failed_to_fetch_intents} | {:ok, any()}
  @doc """
  I advertise to a remote node about my existence, and how it can reach me.
  """
  def advertise(channel, node_id, client_id, grpc_port, grpc_host) do
    # node id of the node I want to subscribe to
    node = %Node{id: node_id}

    # node id of this client
    client_node = %Node{id: client_id}
    # grpc address of this client
    grpc_address = %GRPCAddress{host: grpc_host, port: grpc_port}

    request = %Advertise.Request{
      node: node,
      remote_node: client_node,
      grpc_address: grpc_address
    }

    case AdvertisementService.Stub.advertise(channel, request) do
      {:ok, intents} ->
        {:ok, intents}

      {:error, _} ->
        {:error, :failed_to_fetch_intents}
    end
  end

  @doc """
  I make a call to a GRPC endpoint to retrieve a list of intents from a remote
  node.
  """
  @spec list_intents(any(), String.t()) ::
          {:ok, [binary()]}
          | {:error, :failed_to_fetch_intents}
  def list_intents(channel, node_id) do
    request = %Intentpool.List.Request{node: %Node{id: node_id}}

    case IntentpoolService.Stub.list(channel, request) do
      {:ok, intents} ->
        intents = Enum.map(intents.intents, &Map.get(&1, :intent))
        {:ok, intents}

      {:error, _} ->
        {:error, :failed_to_fetch_intents}
    end
  end

  @doc """
  I make a call to a GRPC endpoint to retrieve a list of intents from a remote
  node.
  """
  @spec add_intent(any(), String.t(), binary()) ::
          {:ok, :added} | {:error, :add_intent_failed, String.t()}
  def add_intent(channel, node_id, intent) do
    node_info = %Node{id: node_id}
    intent = %Intent{intent: intent}
    request = %Intentpool.Add.Request{node: node_info, intent: intent}

    case IntentpoolService.Stub.add(channel, request) do
      {:ok, _} ->
        {:ok, :added}

      {:error, %{status: _, message: err}} ->
        {:error, :add_intent_failed, err}
    end
  end

  @doc """
  I make a call to a GRPC endpoint to add a transaction to the mempool of the
  node.
  """
  @spec add_transaction(any(), String.t(), binary()) ::
          {:ok, :added} | {:error, :add_transaction_failed, String.t()}
  def add_transaction(channel, node_id, transaction) do
    node = %Node{id: node_id}

    transaction = %Transaction{transaction: transaction}

    request = %Mempool.Add.Request{
      node: node,
      transaction: transaction
    }

    case MempoolService.Stub.add(channel, request) do
      {:ok, _} ->
        {:ok, :added}

      {:error, %{status: _, message: err}} ->
        {:error, :add_transaction_failed, err}
    end
  end

  @doc """
  I subscribe to all events for a given topic on the remote node.
  """
  @spec subscribe(any(), String.t(), String.t(), String.t()) ::
          {:ok, :subscribed} | {:error, :subscribe_failed, any()}
  def subscribe(channel, node_id, client_id, topic) do
    node = %Node{id: node_id}

    request = %Subscribe.Request{
      node: node,
      subscriber: %Node{id: client_id},
      topic: %Topic{topic: topic}
    }

    case PubSubService.Stub.subscribe(channel, request) do
      {:ok, _} ->
        {:ok, :subscribed}

      {:error, %{status: _, message: err}} ->
        {:error, :subscribe_failed, err}
    end
  end
end
