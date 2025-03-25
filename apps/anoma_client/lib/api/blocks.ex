defmodule Anoma.Client.Api.Servers.Blocks do
  @moduledoc """
  I implement the callbacks for the GRPC service `Indexer`.
  Each function below implements one API call.
  """

  # alias Anoma.Client.Connection.GRPCProxy
  # alias Anoma.Proto.Indexer.Filtered
  # alias Anoma.Proto.Indexer.Get
  # alias Anoma.Proto.Indexer.Latest
  # alias Anoma.Proto.Indexer.Root
  # alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.IndexerService.Service

  # @doc """
  # I implement the `get` API call.

  # I return a list of blocks from/before the given height.

  # Example request:
  # ```
  # %Anoma.Proto.Indexer.Get.Request{
  #   node_info: %Anoma.Protobuf.NodeInfo{
  #     node_id: "117735458",
  #   },
  #   index: {:before, 2},
  # }
  # ```
  # """
  # @spec get(Get.Request.t(), Stream.t()) :: Get.Response.t()
  # def get(request, _stream) do
  #   {:ok, response} = GRPCProxy.get_blocks(request.index)
  #   %Get.Response{blocks: response.blocks}
  # end

  # @doc """
  # I return the latest block from the indexer.
  # """
  # @spec get(Latest.Request.t(), Stream.t()) :: Latest.Response.t()
  # def latest(_request, _stream) do
  #   {:ok, response} = GRPCProxy.get_latest_block()
  #   %Latest.Response{block: response.block}
  # end

  # @doc """
  # I return the root of the indexer.
  # """
  # @spec root(Root.Request.t(), Stream.t()) :: Root.Response.t()
  # def root(_request, _stream) do
  # {:ok, response} = GRPCProxy.root()
  # %Root.Response{root: response.root}
  # end

  # @doc """
  # I return a list of resources as jammed nouns from the indexer matching the given filters.
  # """
  # @spec filter(Filtered.Request.t(), Stream.t()) :: Filtered.Response.t()
  # def filter(request, _stream) do
  # {:ok, response} = GRPCProxy.filter(request.filters)
  # %Filtered.Response{resources: response.resources}
  # end
end
