defmodule Anoma.Client.Api.Servers.Intents do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Proto.Intentpool.Add
  alias Anoma.Proto.Intentpool.List
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Proto.IntentpoolService.Service

  @spec list(List.Request.t(), Stream.t()) ::
          List.Response.t()
  def list(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    {:ok, intents} = GRPCProxy.list_intents()
    intents
  end

  @spec add(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    {:ok, response} = GRPCProxy.add_intent(request.intent)

    response
  end
end
