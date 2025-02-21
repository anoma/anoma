defmodule Anoma.Client.Api.Servers.Mempool do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Proto.Mempool.Add
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.MempoolService.Service

  require Logger

  @spec add(Add.Request.t(), Stream.t()) :: Add.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    :ok = GRPCProxy.add_transaction(request.transaction)

    %Add.Response{}
  end
end
