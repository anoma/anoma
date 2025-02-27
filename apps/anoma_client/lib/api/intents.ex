defmodule Anoma.Client.Api.Servers.Intents do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.List
  alias GRPC.Server.Stream
  alias Anoma.Protobuf.Intents.Compose
  alias Anoma.Protobuf.Intents.Intent
  alias Anoma.Protobuf.Intents.Verify
  alias Anoma.RM.Transparent.Transaction

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

  @spec compose(Compose.Request.t(), Stream.t()) :: Compose.Response.t()
  def compose(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # fetch the jammed intents from the request
    transactions =
      request.intents
      # pick out the intents from the protobuf intent struct
      |> Enum.map(&Map.get(&1, :intent))
      # cue the jammed intents into nouns
      |> Enum.map(&Noun.Jam.cue!(&1))
      # nouns to transactions
      |> Enum.map(&Transaction.from_noun(&1))
      # unpack {:ok, tx} tuples
      |> Enum.map(fn {:ok, x} -> x end)

    # compose all transactions and then jam them.
    composed =
      transactions
      |> Enum.reduce(&Transaction.compose/2)
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    %Compose.Response{intent: %Intent{intent: composed}}
  end

  @spec verify(Verify.Request.t(), Stream.t()) :: Verify.Response.t()
  def verify(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    valid? =
      request.intent.intent
      |> Noun.Jam.cue!()
      |> Transaction.from_noun()
      |> elem(1)
      |> Anoma.RM.Intent.verify()

    %Verify.Response{valid: valid? == true}
  end
end
