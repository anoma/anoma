defmodule Anoma.Node.Transport.GRPC.Servers.Blocks do
  alias Anoma.Node.Utility.Indexer
  alias Anoma.Protobuf.Indexer.Blocks.Get
  alias Anoma.Protobuf.Indexer.Blocks.Block
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

  ############################################################
  #                       Helpers                            #
  ############################################################

  # @doc """
  # I encode a block from the indexer into a protobuf Block struct.
  # """
  @spec encode_block(any()) :: Block.t()
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
          code: Nock.Jam.jam(transaction.code),
          result: {:success, Nock.Jam.jam(result)}
        }

      %{vm_result: :vm_error} ->
        %Transaction{
          code: Nock.Jam.jam(transaction.code),
          result: {:error, "vm error"}
        }
    end
  end
end
