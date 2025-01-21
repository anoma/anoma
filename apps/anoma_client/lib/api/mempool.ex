defmodule Anoma.Client.Api.Servers.Mempool do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias GRPC.Server.Stream
  alias Anoma.Protobuf.Mempool.Dump

  use GRPC.Server, service: Anoma.Protobuf.MempoolService.Service

  require Logger

  @spec add(AddTransaction.Request.t(), Stream.t()) ::
          AddTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    IO.inspect(request, label: "client")
    :ok = GRPCProxy.add_transaction(request.transaction)

    %AddTransaction.Response{}
  end

  @spec dump(Dump.Request.t(), Stream.t()) :: Dump.Response.t()
  def dump(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    {:ok, response} = GRPCProxy.dump_mempool()

    %Dump.Response{transaction_candidates: response.transaction_candidates}
  end
end
