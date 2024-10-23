defmodule Anoma.Node.Transport.GRPC.Server do
  alias Anoma.Node.Registry
  alias Anoma.RM.DumbIntent
  alias Anoma.Node.Utility.Indexer
  alias GRPC.Server.Stream
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.IntentPool.AddIntent
  alias Anoma.Protobuf.IntentPool.ListIntents
  alias Anoma.Protobuf.Intents

  use GRPC.Server, service: Intents.Service

  require Logger

  @spec list_intents(ListIntents.Request.t(), Stream.t()) ::
          ListIntents.Response.t()
  def list_intents(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    # get the intents from the intenet pool and turn them into
    # protobuf structs
    {:ok, local_node_id} = Registry.local_node_id()

    intents =
      Anoma.Node.Intents.IntentPool.intents(local_node_id)
      |> Enum.map(&inspect(&1.value))

    %ListIntents.Response{intents: intents}
  end

  @spec add_intent(AddIntent.Request.t(), Stream.t()) ::
          AddIntent.Response.t()
  def add_intent(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    new_intent = %DumbIntent{value: request.intent.value}
    {:ok, local_node_id} = Registry.local_node_id()
    Anoma.Node.Intents.IntentPool.new_intent(local_node_id, new_intent)

    %AddIntent.Response{result: "intent added"}
  end

  @spec list_nullifiers(Nullifiers.Request.t(), Stream.t()) ::
          Nullifiers.Response.t()
  def list_nullifiers(_request, _stream) do
    {:ok, local_node_id} = Registry.local_node_id()
    nullifiers = Indexer.get(local_node_id, :nlfs)
    %Nullifiers.Response{nullifiers: nullifiers}
  end

  @spec list_unrevealed_commits(UnrevealedCommits.Request.t(), Stream.t()) ::
          UnrevealedCommits.Response.t()
  def list_unrevealed_commits(_request, _stream) do
    {:ok, local_node_id} = Registry.local_node_id()
    commits = Indexer.get(local_node_id, :cms)
    %UnrevealedCommits.Response{commits: commits}
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(_request, _stream) do
    {:ok, local_node_id} = Registry.local_node_id()
    resources = Indexer.get(local_node_id, :resources)

    %UnspentResources.Response{unspent_resources: resources}
  end
end
