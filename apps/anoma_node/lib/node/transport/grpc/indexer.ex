defmodule Anoma.Node.Transport.GRPC.Servers.Indexer do
  alias Anoma.Node.Registry
  alias Anoma.Node.Utility.Indexer
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.IndexerService.Service

  require Logger

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
