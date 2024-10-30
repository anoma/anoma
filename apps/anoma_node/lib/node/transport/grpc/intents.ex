defmodule Anoma.Node.Transport.GRPC.Servers.Intents do
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.List
  alias Anoma.RM.DumbIntent
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.IntentsService.Service

  require Logger

  @spec list_intents(List.Request.t(), Stream.t()) :: List.Response.t()
  def list_intents(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    intents =
      IntentPool.intents(request.node_info.node_id)
      |> Enum.map(&inspect(&1.value))

    %List.Response{intents: intents}
  end

  @spec add_intent(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add_intent(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    new_intent = %DumbIntent{value: request.intent.value}
    IntentPool.new_intent(request.node_info.node_id, new_intent)

    %Add.Response{result: "intent added"}
  end
end
