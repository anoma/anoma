defmodule Anoma.Client.Api.Servers.Mempool do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Protobuf.MempoolService.Service

  @spec add(AddTransaction.Request.t(), Stream.t()) ::
          AddTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    :ok = GRPCProxy.add_transaction(request.transaction)

    %AddTransaction.Response{}
  end
end
