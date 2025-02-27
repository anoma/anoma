defmodule Anoma.Node.Transport.GRPC.Servers.Indexer do
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Protobuf.IndexerService.Service

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
end
