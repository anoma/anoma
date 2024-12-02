defmodule Anoma.Node.Transport.GRPC.Servers.Mempool do
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias Anoma.Protobuf.Mempool.Dump
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.MempoolService.Service

  require Logger

  @spec add(AddTransaction.Request.t(), Stream.t()) ::
          AddTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    tx_noun = request.transaction |> Noun.Jam.cue!()

    Mempool.tx(request.node_info.node_id, {:transparent_resource, tx_noun})

    %AddTransaction.Response{}
  end

  @spec add(Dump.Request.t(), Stream.t()) :: Dump.Response.t()
  def dump(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    jammed_transaction_candidates =
      Mempool.tx_dump_txs(request.node_info.node_id)
      |> Enum.map(&Map.get(&1, :code))
      |> Enum.map(&Noun.Jam.jam/1)

    %Dump.Response{transaction_candidates: jammed_transaction_candidates}
  end
end
