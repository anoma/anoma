defmodule Anoma.Node.Transport.GRPC.Servers.Mempool do
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias GRPC.Server.Stream
  alias Noun.Jam

  use GRPC.Server, service: Anoma.Protobuf.MempoolService.Service

  require Logger

  import Anoma.Protobuf.ErrorHandler

  @spec add(AddTransaction.Request.t(), Stream.t()) ::
          AddTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node_info.node_id, Mempool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    # create the transaction from the noun
    noun = Jam.cue!(request.transaction)
    transaction = {:transparent_resource, noun}

    # submit the transaction to the mempool
    node_id = request.node_info.node_id
    :ok = Mempool.tx(node_id, transaction)

    # return an empty response
    %AddTransaction.Response{}
  rescue
    e -> raise_grpc_error!(e)
  end
end
