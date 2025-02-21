defmodule Anoma.Node.Transport.GRPC.Servers.Intents do
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Proto.Intentpool.Intent
  alias Anoma.Proto.Intentpool.Add
  alias Anoma.Proto.Intentpool.List
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.IntentpoolService.Service

  require Logger

  @spec list(List.Request.t(), Stream.t()) :: List.Response.t()
  def list(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    intents =
      IntentPool.intents(request.node.id)
      |> Enum.map(fn i ->
        i
        |> Noun.Nounable.to_noun()
        |> Noun.Jam.jam()
      end)
      |> Enum.map(&%Intent{intent: &1})

    %List.Response{intents: intents}
  end

  @spec add(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # the input is a jammed intent.
    #  cue it and create a transaction
    {:ok, intent} =
      request.intent.intent
      |> Noun.Jam.cue!()
      |> Anoma.TransparentResource.Transaction.from_noun()

    IntentPool.new_intent(request.node.id, intent)

    %Add.Response{result: "intent added"}
  end
end
