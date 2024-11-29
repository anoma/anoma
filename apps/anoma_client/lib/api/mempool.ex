defmodule Anoma.Client.Api.Servers.Mempool do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias Anoma.Protobuf.Mempool.Dump
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.MempoolService.Service

  require Logger

  @spec add(AddTransaction.Request.t(), Stream.t()) ::
          AddTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    :ok = GRPCProxy.add_transaction(request.transaction)

    %AddTransaction.Response{}
  end

  @spec add(Dump.Request.t(), Stream.t()) ::
          Dump.Response.t()
  def dump(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    {:ok, response} = GRPCProxy.dump_mempool()

    %Dump.Response{transaction_candidates: response.transaction_candidates}
  end
end
