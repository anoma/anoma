defmodule Anoma.Client.Api.Servers.Executor do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Executor.AddROTransaction
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.ExecutorService.Service

  require Logger

  @spec add(AddROTransaction.Request.t(), Stream.t()) ::
          AddROTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    {:ok, response} = GRPCProxy.add_read_only_transaction(request.transaction)

    response
  end
end
