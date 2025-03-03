defmodule Anoma.Node.Transport.GRPC.Servers.Mempool do
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Protobuf.MempoolService.Service

  @spec add(AddTransaction.Request.t(), Stream.t()) ::
          AddTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    tx_noun = request.transaction |> Noun.Jam.cue!()

    Mempool.tx(request.node_info.node_id, {:transparent_resource, tx_noun})

    %AddTransaction.Response{}
  end
end
