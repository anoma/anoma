defmodule Anoma.Node.Transport.GRPC.Servers.Mempool do
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias GRPC.Server.Stream
  alias Anoma.Node.Transaction.Mempool

  use GRPC.Server, service: Anoma.Protobuf.MempoolService.Service

  require Logger

  @spec add(AddTransaction.Request.t(), Stream.t()) ::
          AddTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    tx_noun = request.transaction |> Noun.Jam.cue!()

    Mempool.tx(request.node_info.node_id, {request.transaction_type, tx_noun})

    %AddTransaction.Response{}
  end
end
