defmodule Anoma.Node.Transport.GRPC.Servers.Indexer do
  alias Anoma.Node.Utility.Indexer
  alias Anoma.Proto.Indexer.Block
  alias Anoma.Proto.Indexer.Commits
  alias Anoma.Proto.Indexer.FilterResource
  alias Anoma.Proto.Indexer.GetBlock
  alias Anoma.Proto.Indexer.LatestBlock
  alias Anoma.Proto.Indexer.Nullifiers
  alias Anoma.Proto.Indexer.RootBlock
  alias Anoma.Proto.Indexer.Transaction
  alias Anoma.Proto.Indexer.UnrevealedCommits
  alias Anoma.Proto.Indexer.UnspentResources
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.IndexerService.Service

  require Logger

  @spec list_nullifiers(Nullifiers.Request.t(), Stream.t()) ::
          Nullifiers.Response.t()
  def list_nullifiers(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    nullifiers = Indexer.get(request.node.id, :nlfs)

    %Nullifiers.Response{nullifiers: nullifiers}
  end

  @spec list_unrevealed_commits(UnrevealedCommits.Request.t(), Stream.t()) ::
          UnrevealedCommits.Response.t()
  def list_unrevealed_commits(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    unrevealed = Indexer.get(request.node.id, :unrevealed)

    %UnrevealedCommits.Response{commits: unrevealed}
  end

  @spec list_commits(Commits.Request.t(), Stream.t()) :: Commits.Response.t()
  def list_commits(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    unrevealed = Indexer.get(request.node.id, :cms)

    %Commits.Response{commits: unrevealed}
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    resources =
      Indexer.get(request.node.id, :resources)
      |> Enum.map(fn r ->
        Noun.Nounable.to_noun(r)
        |> Noun.Jam.jam()
      end)

    %UnspentResources.Response{unspent_resources: resources}
  end

  @doc """
  I return the blocks requested by the client.

  Example request:

  ```
  %Anoma.Proto.Indexer.Get.Request{
    node_info: %Anoma.Protobuf.NodeInfo{
      node_id: "117735458",
    },
    index: {:before, 2},
  }
  ```
  """
  @spec get_block(GetBlock.Request.t(), Stream.t()) :: GetBlock.Response.t()
  def get_block(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # fetch the blocks from the indexer and encode each block and its transactions into the protobuf structs
    blocks =
      Indexer.get(request.node.id, request.index)
      |> Enum.map(&encode_block/1)

    %GetBlock.Response{blocks: blocks}
  end

  @doc """
  I return the latest block from the indexer.
  """
  @spec latest_block(LatestBlock.Request.t(), Stream.t()) ::
          LatestBlock.Response.t()
  def latest_block(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # fetch the blocks from the indexer and encode each block and its transactions into the protobuf structs
    block =
      Indexer.get(request.node.id, :latest_block)
      # get returns a list with 1 block, or nil
      |> case do
        nil -> nil
        [block] -> encode_block(block)
      end

    %LatestBlock.Response{block: block}
  end

  @doc """
  I return the root of the indexer.
  """
  @spec root_block(RootBlock.Request.t(), Stream.t()) ::
          RootBlock.Response.t()
  def root_block(request, _stream) do
    root = Indexer.get(request.node.id, :root)

    %RootBlock.Response{root: root}
  end

  def filter_resource(request, _stream) do
    # extract the filters from the request
    # A filter is of the type {:filter, [{:owner, any()} | {:kind, binary()}]}
    filters =
      request.filters
      |> Enum.map(fn %{filter: filter} -> filter end)

    resources =
      Indexer.get(request.node.id, {:filter, filters})
      |> Enum.map(&encode_resource/1)

    %FilterResource.Response{resources: resources}
  end

  ############################################################
  #                       Helpers                            #
  ############################################################

  # @doc """
  # Given a resource (a noun) I jam it into a binary.
  # """
  @spec encode_resource(Noun.t()) :: binary()
  defp encode_resource(resource) do
    Noun.Jam.jam(resource)
  end

  # @doc """
  # I encode a block from the indexer into a protobuf Block struct.
  # If there is no block, I return nil
  # """
  @spec encode_block(any()) :: Block.t()
  defp encode_block(nil) do
    nil
  end

  defp encode_block([height, transactions]) do
    transactions = Enum.map(transactions, &encode_transaction/1)
    %Block{transactions: transactions, height: height}
  end

  # @doc """
  # I encode a transaction into the protobuf Transaction struct.
  # """
  @spec encode_transaction(any()) :: Transaction.t()
  defp encode_transaction(transaction) do
    case transaction do
      %{vm_result: {:ok, result}} ->
        %Transaction{
          code: Noun.Jam.jam(transaction.code),
          result: {:success, Noun.Jam.jam(result)}
        }

      %{vm_result: :vm_error} ->
        %Transaction{
          code: Noun.Jam.jam(transaction.code),
          result: {:error, "vm error"}
        }
    end
  end
end
