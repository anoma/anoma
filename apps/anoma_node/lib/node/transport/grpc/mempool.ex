defmodule Anoma.Node.Transport.GRPC.Servers.Mempool do
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Proto.Mempool.Add
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.MempoolService.Service
  require Logger

  @spec add(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    tx_noun = request.transaction |> Noun.Jam.cue!()

    Mempool.tx(request.node.id, {:transparent_resource, tx_noun})

    %Add.Response{}
  end
end
