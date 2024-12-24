defmodule Anoma.Node.Transport.GRPC.Servers.Blocks do
  alias Anoma.Node.Utility.Indexer
  alias Anoma.Protobuf.Indexer.Blocks.Block
  alias Anoma.Protobuf.Indexer.Blocks.Filtered
  alias Anoma.Protobuf.Indexer.Blocks.Get
  alias Anoma.Protobuf.Indexer.Blocks.Latest
  alias Anoma.Protobuf.Indexer.Blocks.Root
  alias Anoma.Protobuf.Indexer.Blocks.Transaction
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.BlockService.Service

  require Logger

  @doc """
  I return the blocks requested by the client.

  Example request:

  ```
  %Anoma.Protobuf.Indexer.Blocks.Get.Request{
    node_info: %Anoma.Protobuf.NodeInfo{
      node_id: "117735458",
    },
    index: {:before, 2},
  }
  ```
  """
  @spec get(Get.Request.t(), Stream.t()) :: Get.Response.t()
  def get(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # fetch the blocks from the indexer and encode each block and its transactions into the protobuf structs
    blocks =
      Indexer.get(request.node_info.node_id, request.index)
      |> Enum.map(&encode_block/1)

    %Get.Response{blocks: blocks}
  end

  @doc """
  I return the latest block from the indexer.
  """
  @spec get(Latest.Request.t(), Stream.t()) :: Latest.Response.t()
  def latest(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # fetch the blocks from the indexer and encode each block and its transactions into the protobuf structs
    block =
      Indexer.get(request.node_info.node_id, :latest_block)
      # get returns a list with 1 block, or nil
      |> case do
        nil -> nil
        [block] -> encode_block(block)
      end

    %Latest.Response{block: block}
  end

  @doc """
  I return the root of the indexer.
  """
  @spec root(Root.Request.t(), Stream.t()) :: Root.Response.t()
  def root(request, _stream) do
    root = Indexer.get(request.node_info.node_id, :root)

    %Root.Response{root: root}
  end

  def filter(request, _stream) do
    # extract the filters from the request
    # A filter is of the type {:filter, [{:owner, any()} | {:kind, binary()}]}
    filters =
      request.filters
      |> Enum.map(fn %{filter: filter} -> filter end)

    resources =
      Indexer.get(request.node_info.node_id, {:filter, filters})
      |> Enum.map(&encode_resource/1)

    %Filtered.Response{resources: resources}
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
