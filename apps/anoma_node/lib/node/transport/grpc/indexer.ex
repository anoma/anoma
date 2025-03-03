defmodule Anoma.Node.Transport.GRPC.Servers.Indexer do
  alias Anoma.Node.Utility.Indexer
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.Commits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.IndexerService.Service
  require Logger

  @spec list_nullifiers(Nullifiers.Request.t(), Stream.t()) ::
          Nullifiers.Response.t()
  def list_nullifiers(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    nullifiers = Indexer.get(request.node_info.node_id, :nlfs) |> Enum.map(&Noun.atom_integer_to_binary/1)

    %Nullifiers.Response{nullifiers: nullifiers}
  end

  @spec list_unrevealed_commits(UnrevealedCommits.Request.t(), Stream.t()) ::
          UnrevealedCommits.Response.t()
  def list_unrevealed_commits(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    unrevealed = Indexer.get(request.node_info.node_id, :unrevealed) |> Enum.map(&Noun.atom_integer_to_binary/1)

    %UnrevealedCommits.Response{commits: unrevealed}
  end

  @spec list_commits(Commits.Request.t(), Stream.t()) :: Commits.Response.t()
  def list_commits(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    unrevealed = Indexer.get(request.node_info.node_id, :cms) |> Enum.map(&Noun.atom_integer_to_binary/1)

    %Commits.Response{commits: unrevealed}
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    resources =
      Indexer.get(request.node_info.node_id, :resources)
      |> Enum.map(fn r ->
        Noun.Nounable.to_noun(r)
        |> Noun.Jam.jam()
      end)

    %UnspentResources.Response{unspent_resources: resources}
  end
end
