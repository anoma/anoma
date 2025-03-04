defmodule Anoma.Node.Transport.GRPC.Servers.Intents do
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.List
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Protobuf.IntentsService.Service

  @spec list_intents(List.Request.t(), Stream.t()) :: List.Response.t()
  def list_intents(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    intents =
      IntentPool.intents(request.node_info.node_id)
      |> Enum.map(fn i ->
        i
        |> Noun.Nounable.to_noun()
        |> Noun.Jam.jam()
      end)

    %List.Response{intents: intents}
  end

  @spec add_intent(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add_intent(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # the input is a jammed intent.
    #  cue it and create a transaction
    {:ok, intent} =
      request.intent.intent
      |> Noun.Jam.cue!()
      |> Anoma.RM.Transparent.Transaction.from_noun()

    IntentPool.new_intent(request.node_info.node_id, intent)

    %Add.Response{result: "intent added"}
  end
end
