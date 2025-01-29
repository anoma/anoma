defmodule Anoma.Client.Api.Servers.Intents do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.List
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Protobuf.IntentsService.Service

  import Anoma.Protobuf.ErrorHandler

  @spec list_intents(List.Request.t(), Stream.t()) ::
          List.Response.t()
  def list_intents(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    case GRPCProxy.list_intents() do
      {:ok, intents} ->
        intents

      {:error, grpc_error} ->
        raise grpc_error
    end
  end

  @spec add_intent(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add_intent(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    case GRPCProxy.add_intent(request.intent) do
      {:ok, response} ->
        response

      {:error, grpc_error} ->
        raise grpc_error
    end
  end
end
