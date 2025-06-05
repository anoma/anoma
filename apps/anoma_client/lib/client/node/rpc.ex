defmodule Anoma.Client.Node.RPC do
  @moduledoc """
  I contain functions that make requests to the GRPC endpoint of a node.
  """

  alias Anoma.Proto.Advertisement.Advertise
  alias Anoma.Proto.Advertisement.GRPCAddress
  alias Anoma.Proto.AdvertisementService
  alias Anoma.Proto.Executor.AddROTransaction
  alias Anoma.Proto.ExecutorService
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

  alias Anoma.Proto.Indexer.Commits
  alias Anoma.Proto.Indexer.Filter
  alias Anoma.Proto.Indexer.FilterResource
  alias Anoma.Proto.Indexer.GetBlock
  alias Anoma.Proto.Indexer.LatestBlock
  alias Anoma.Proto.Indexer.Nullifiers
  alias Anoma.Proto.Indexer.RootBlock
  alias Anoma.Proto.Indexer.UnrevealedCommits
  alias Anoma.Proto.Indexer.UnspentResources
  alias Anoma.Proto.IndexerService

  @doc """
  I advertise to a remote node about my existence, and how it can reach me.
  """
  @spec advertise(GRPC.Channel.t(), any(), any(), any(), any()) ::
          {:error, :failed_to_fetch_intents} | {:ok, any()}
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
  @spec add_transaction(any(), String.t(), binary(), atom(), boolean()) ::
          {:ok, :added} | {:error, :add_transaction_failed, String.t()}
  def add_transaction(channel, node_id, transaction, transaction_type, wrap) do
    node = %Node{id: node_id}

    transaction = %Transaction{transaction: transaction}

    request = %Mempool.Add.Request{
      node: node,
      transaction: transaction,
      transaction_type: transaction_type,
      wrap: wrap
    }

    case MempoolService.Stub.add(channel, request) do
      {:ok, _} ->
        {:ok, :added}

      {:error, %{status: _, message: err}} ->
        {:error, :add_transaction_failed, err}
    end
  end

  @doc """
  I make a call to a GRPC endpoint to add a read-only transaction to the mempool of the
  node.

  The result of this call is either an error, or a jammed noun.
  """
  @spec add_read_only_transaction(any(), String.t(), binary()) ::
          {:ok, Noun.t()}
          | {:error, :add_read_only_transaction_failed, String.t()}
          | {:error, :absent}
  def add_read_only_transaction(channel, node_id, transaction) do
    node = %Node{id: node_id}

    transaction = %Transaction{transaction: transaction}

    request = %AddROTransaction.Request{
      node: node,
      transaction: transaction
    }

    case ExecutorService.Stub.add(channel, request) do
      {:ok, %AddROTransaction.Response{result: result}} ->
        case result do
          {:success, %{result: jammed_nock}} ->
            {:ok, Noun.Jam.cue!(jammed_nock)}

          {:error, %{error: "absent"}} ->
            {:error, :absent}
        end

      {:error, %{status: _, message: err}} ->
        {:error, :add_read_only_transaction_failed, err}
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

  # ----------------------------------------------------------------------------
  # Testnet

  @type block :: %{
          height: non_neg_integer,
          transactions: [transaction()]
        }

  @type transaction :: %{code: binary(), result: {:success, binary()}}

  @spec list_nullifiers(any(), String.t()) ::
          {:ok, [binary()]} | {:error, :failed_to_list_nullifiers, String.t()}
  def list_nullifiers(channel, node_id) do
    node_info = %Node{id: node_id}

    request = %Nullifiers.Request{node: node_info}

    case IndexerService.Stub.list_nullifiers(channel, request) do
      {:ok, %{nullifiers: nullifiers}} ->
        {:ok, nullifiers}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_list_nullifiers, err}
    end
  end

  @spec list_unrevealed_commits(any(), String.t()) ::
          {:ok, [binary()]}
          | {:error, :failed_to_list_unrevealed_commits, String.t()}
  def list_unrevealed_commits(channel, node_id) do
    node_info = %Node{id: node_id}

    request = %UnrevealedCommits.Request{node: node_info}

    case IndexerService.Stub.list_unrevealed_commits(channel, request) do
      {:ok, %{commits: commits}} ->
        {:ok, commits}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_list_unrevealed_commits, err}
    end
  end

  @spec list_commits(any(), String.t()) ::
          {:ok, [non_neg_integer()]}
          | {:error, :failed_to_list_commits, String.t()}
  def list_commits(channel, node_id) do
    node_info = %Node{id: node_id}

    request = %Commits.Request{node: node_info}

    case IndexerService.Stub.list_commits(channel, request) do
      {:ok, %{commits: commits}} ->
        commits = Enum.map(commits, &:binary.decode_unsigned(&1))
        {:ok, commits}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_list_commits, err}
    end
  end

  @spec list_unspent_resources(any(), String.t()) ::
          {:ok, [binary()]}
          | {:error, :failed_to_list_unspent_resources, String.t()}
  def list_unspent_resources(channel, node_id) do
    node_info = %Node{id: node_id}

    request = %UnspentResources.Request{node: node_info}

    case IndexerService.Stub.list_unspent_resources(channel, request) do
      {:ok, %{unspent_resources: unspent_resources}} ->
        {:ok, unspent_resources}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_list_unspent_resources, err}
    end
  end

  @spec get_blocks(any(), String.t(), :before | :after, non_neg_integer()) ::
          {:ok, [block()]} | {:error, :failed_to_get_blocks, String.t()}
  def get_blocks(channel, node_id, direction, offset) do
    node_info = %Node{id: node_id}

    request = %GetBlock.Request{node: node_info, index: {direction, offset}}

    case IndexerService.Stub.get_block(channel, request) do
      {:ok, %{blocks: blocks}} ->
        blocks =
          blocks
          |> Enum.map(fn block ->
            %{
              height: block.height,
              transactions:
                Enum.map(block.transactions, fn t ->
                  %{code: t.code, result: t.result}
                end)
            }
          end)

        {:ok, blocks}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_get_blocks, err}
    end
  end

  @spec latest_block(any(), String.t()) ::
          {:ok, block() | nil} | {:error, :failed_to_get_block, String.t()}

  def latest_block(channel, node_id) do
    node_info = %Node{id: node_id}
    request = %LatestBlock.Request{node: node_info}

    case IndexerService.Stub.latest_block(channel, request) do
      {:ok, %{block: nil}} ->
        {:ok, nil}

      {:ok, %{block: block}} ->
        block = %{
          height: block.height,
          transactions:
            Enum.map(block.transactions, fn t ->
              %{code: t.code, result: t.result}
            end)
        }

        {:ok, block}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_get_block, err}
    end
  end

  @spec root_block(any(), String.t()) ::
          {:ok, binary()} | {:error, :failed_to_get_root, String.t()}
  def root_block(channel, node_id) do
    node_info = %Node{id: node_id}
    request = %RootBlock.Request{node: node_info}

    case IndexerService.Stub.root_block(channel, request) do
      {:ok, %{root: root}} ->
        {:ok, root}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_get_root, err}
    end
  end

  @spec filter(any(), String.t(), [{:owner | :kind, binary()}]) ::
          {:ok, [binary()]}
          | {:error, :failed_to_filter_resources, String.t()}
  def filter(channel, node_id, filters) do
    # encode the filters
    filters =
      Enum.map(filters, fn
        {:owner, owner} ->
          %Filter{filter: {:owner, owner}}

        {:kind, kind} ->
          %Filter{filter: {:kind, kind}}
      end)

    node_info = %Node{id: node_id}
    request = %FilterResource.Request{node: node_info, filters: filters}

    case IndexerService.Stub.filter_resource(channel, request) do
      {:ok, %{resources: resources}} ->
        {:ok, resources}

      {:error, %{status: _, message: err}} ->
        {:error, :failed_to_filter_resources, err}
    end
  end
end
