defmodule Anoma.Node.Transport.GRPC.Servers.IntraNode do
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Proto.IntraNode.Call
  alias Anoma.Proto.IntraNode.Cast
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.IntraNodeService.Service

  require Logger

  import Anoma.Protobuf.ErrorHandler

  @spec call(Call.Request.t(), Stream.t()) :: Call.Response.t()
  def call(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node.id, Mempool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    engine = String.to_atom(request.engine)
    name = Registry.via(request.node.id, engine)
    message = :erlang.binary_to_term(request.message)

    result = GenServer.call(name, message)
    %Call.Response{message: :erlang.term_to_binary(result)}
  end

  @spec cast(Cast.Request.t(), Stream.t()) :: Cast.Response.t()
  def cast(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node.id, Mempool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    engine = String.to_atom(request.engine)
    name = Registry.via(request.node.id, engine)
    message = :erlang.binary_to_term(request.message)

    GenServer.cast(name, message)
    %Cast.Response{}
  end
end
