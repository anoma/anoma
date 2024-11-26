defmodule Anoma.Client.Api.Servers.Indexer do
  @moduledoc """
  I implement the callbacks for the GRPC service `Indexer`.
  Each function below implements one API call.
  """
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.IndexerService.Service

  @spec list_nullifiers(Nullifiers.Request.t(), Stream.t()) ::
          Nullifiers.Response.t()
  def list_nullifiers(_request, _stream) do
    {:ok, nullifiers} = GRPCProxy.list_nullifiers()
    nullifiers
  end

  @spec list_unrevealed_commits(UnrevealedCommits.Request.t(), Stream.t()) ::
          UnrevealedCommits.Response.t()
  def list_unrevealed_commits(_request, _stream) do
    {:ok, commits} = GRPCProxy.list_unrevealed_commits()
    commits
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(_request, _stream) do
    {:ok, resources} = GRPCProxy.list_unspent_resources()
    resources
  end
end
