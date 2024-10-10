defmodule Anoma.Node.Transport.GRPC.Server do
  alias GRPC.Server.Stream
  alias Protobufs.Indexer.Nullifiers
  alias Protobufs.Indexer.UnrevealedCommits
  alias Protobufs.Indexer.UnspentResources
  alias Protobufs.IntentPool.AddIntent
  alias Protobufs.IntentPool.ListIntents
  alias Protobufs.Intents
  alias Protobufs.Prove

  use GRPC.Server, service: Intents.Service

  require Logger

  @spec list_intents(ListIntents.Request.t(), Stream.t()) ::
          ListIntents.Response.t()
  def list_intents(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    %ListIntents.Response{intents: []}
  end

  @spec add_intent(AddIntent.Request.t(), Stream.t()) ::
          AddIntent.Response.t()
  def add_intent(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    %AddIntent.Response{result: "intent added"}
  end

  @spec list_nullifiers(Nullifiers.Request.t(), Stream.t()) ::
          Nullifiers.Response.t()
  def list_nullifiers(_request, _stream) do
    %Nullifiers.Response{nullifiers: ["null", "ifier"]}
  end

  @spec list_unrevealed_commits(UnrevealedCommits.Request.t(), Stream.t()) ::
          UnrevealedCommits.Response.t()
  def list_unrevealed_commits(_request, _stream) do
    %UnrevealedCommits.Response{commits: ["commit1", "commit2"]}
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(_request, _stream) do
    %UnspentResources.Response{
      unspent_resources: ["unspent resource 1", "unspent resource 2"]
    }
  end

  @spec prove(Prove.Request.t(), Stream.t()) ::
          Prove.Response.t()
  def prove(request, _stream) do
    %Prove.Response{result: "here's your proof for #{request.intent}"}
  end
end
