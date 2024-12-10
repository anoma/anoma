defmodule Anoma.Node.Transport.GRPC.Servers.Indexer do
  alias Anoma.Node.Utility.Indexer
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.TransparentResource.Resource
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.IndexerService.Service

  require Logger

  @spec list_nullifiers(Nullifiers.Request.t(), Stream.t()) ::
          Nullifiers.Response.t()
  def list_nullifiers(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    nullifiers = Indexer.get(request.node_info.node_id, :nlfs)

    %Nullifiers.Response{nullifiers: nullifiers}
  end

  @spec list_unrevealed_commits(UnrevealedCommits.Request.t(), Stream.t()) ::
          UnrevealedCommits.Response.t()
  def list_unrevealed_commits(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    unrevealed = Indexer.get(request.node_info.node_id, :unrevealed)

    %UnrevealedCommits.Response{commits: unrevealed}
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    resources =
      request.node_info.node_id
      |> Indexer.unspent_resources()
      |> Enum.map(&Resource.to_noun/1)
      |> Enum.map(&Nock.Jam.jam/1)

    %UnspentResources.Response{unspent_resources: resources}
  end
end
