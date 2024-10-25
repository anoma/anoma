defmodule Anoma.Client.Api.Servers.Intents do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.List
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Protobuf.IntentsService.Service

  @spec list_intents(List.Request.t(), Stream.t()) ::
          List.Response.t()
  def list_intents(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    {:ok, intents} = GRPCProxy.list_intents()
    intents
  end

  @spec add_intent(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add_intent(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    {:ok, response} = GRPCProxy.add_intent(request.intent)

    response
  end
end
