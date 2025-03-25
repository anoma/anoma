defmodule Anoma.Client.Api.Servers.Indexer do
  @moduledoc """
  I implement the callbacks for the GRPC service `Indexer`.
  Each function below implements one API call.
  """
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Proto.Indexer.Commits
  alias Anoma.Proto.Indexer.FilterResource
  alias Anoma.Proto.Indexer.GetBlock
  alias Anoma.Proto.Indexer.LatestBlock
  alias Anoma.Proto.Indexer.Nullifiers
  alias Anoma.Proto.Indexer.RootBlock
  alias Anoma.Proto.Indexer.UnrevealedCommits
  alias Anoma.Proto.Indexer.UnspentResources

  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.IndexerService.Service

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

  @spec list_commits(Commits.Request.t(), Stream.t()) :: Commits.Response.t()
  def list_commits(_request, _stream) do
    {:ok, commits} = GRPCProxy.list_commits()
    commits
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(_request, _stream) do
    {:ok, resources} = GRPCProxy.list_unspent_resources()
    resources
  end

  @spec get_block(GetBlock.Request.t(), Stream.t()) :: GetBlock.Response.t()
  def get_block(request, _stream) do
    {:ok, response} = GRPCProxy.get_blocks(request.index)
    %GetBlock.Response{blocks: response.blocks}
  end

  @doc """
  I return the latest block from the indexer.
  """
  @spec latest_block(LatestBlock.Request.t(), Stream.t()) ::
          LatestBlock.Response.t()
  def latest_block(_request, _stream) do
    {:ok, response} = GRPCProxy.get_latest_block()
    %LatestBlock.Response{block: response.block}
  end

  @doc """
  I return the root of the indexer.
  """
  @spec root_block(RootBlock.Request.t(), Stream.t()) ::
          RootBlock.Response.t()
  def root_block(_request, _stream) do
    {:ok, response} = GRPCProxy.root()
    %RootBlock.Response{root: response.root}
  end

  def filter_resource(request, _stream) do
    {:ok, response} = GRPCProxy.filter(request.filters)
    %FilterResource.Response{resources: response.resources}
  end
end
