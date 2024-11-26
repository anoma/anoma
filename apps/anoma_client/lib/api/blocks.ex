defmodule Anoma.Client.Api.Servers.Blocks do
  @moduledoc """
  I implement the callbacks for the GRPC service `Indexer`.
  Each function below implements one API call.
  """
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Indexer.Blocks.Get
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.BlockService.Service

  @doc """
  I implement the `get` API call.

  I return a list of blocks from/before the given height.

  Example request:
  ```
  %Anoma.Protobuf.Indexer.Blocks.Get.Request{
    node_info: %Anoma.Protobuf.NodeInfo{
      node_id: "117735458",
    },
    index: {:before, 2},
  }
  ```
  """
  @spec get(Get.Request.t(), Stream.t()) :: Get.Response.t()
  def get(request, _stream) do
    {:ok, response} = GRPCProxy.get_blocks(request.index)
    %Get.Response{blocks: response.blocks}
  end
end
