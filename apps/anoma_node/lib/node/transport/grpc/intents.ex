defmodule Anoma.Node.Transport.GRPC.Servers.Intents do
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Node.Registry
  alias Anoma.Proto.Intentpool.Add
  alias Anoma.Proto.Intentpool.Intent
  alias Anoma.Proto.Intentpool.List
  alias Anoma.TransparentResource.Transaction
  alias GRPC.Server.Stream
  alias Noun.Jam
  alias Noun.Nounable

  use GRPC.Server, service: Anoma.Proto.IntentpoolService.Service

  require Logger

  import Anoma.Protobuf.ErrorHandler

  @spec list(List.Request.t(), Stream.t()) :: List.Response.t()
  def list(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node.id, IntentPool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    intents =
      IntentPool.intents(request.node.id)
      |> Enum.map(&Nounable.to_noun/1)
      |> Enum.map(&Jam.jam/1)
      |> Enum.map(&%Intent{intent: &1})

    %List.Response{intents: intents}
  end

  @spec add(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node.id, IntentPool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    # the input is a jammed intent. cue it and create a transaction
    {:ok, intent} =
      request.intent.intent
      |> Jam.cue!()
      |> Transaction.from_noun()

    IntentPool.new_intent(request.node.id, intent)

    %Add.Response{result: "intent added"}
  rescue
    e ->
      raise_grpc_error!(e)
  end
end
