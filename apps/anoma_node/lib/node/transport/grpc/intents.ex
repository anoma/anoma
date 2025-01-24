defmodule Anoma.Node.Transport.GRPC.Servers.Intents do
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Node.Registry
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.List
  alias Anoma.TransparentResource.Transaction
  alias GRPC.Server.Stream
  alias Noun.Jam
  alias Noun.Nounable

  use GRPC.Server, service: Anoma.Protobuf.IntentsService.Service

  require Logger

  import Anoma.Protobuf.ErrorHandler

  @spec list_intents(List.Request.t(), Stream.t()) :: List.Response.t()
  def list_intents(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node_info.node_id, IntentPool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    intents =
      IntentPool.intents(request.node_info.node_id)
      |> Enum.map(&Nounable.to_noun/1)
      |> Enum.map(&Jam.jam/1)

    %List.Response{intents: intents}
  end

  @spec add_intent(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add_intent(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node_info.node_id, IntentPool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    # the input is a jammed intent. cue it and create a transaction
    {:ok, intent} =
      request.intent.intent
      |> Jam.cue!()
      |> Transaction.from_noun()

    IntentPool.new_intent(request.node_info.node_id, intent)

    %Add.Response{result: "intent added"}
  rescue
    e ->
      # if the nock was invalid, this will raise a Noun.Jam.CueError.
      raise_grpc_error!(e)
  end
end
