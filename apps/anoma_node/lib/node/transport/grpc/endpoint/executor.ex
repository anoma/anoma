defmodule Anoma.Node.Transport.GRPC.Servers.Executor do
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Proto.Executor.AddROTransaction
  alias Anoma.Proto.Nock.Error
  alias Anoma.Proto.Nock.Success
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.ExecutorService.Service

  import Anoma.Protobuf.ErrorHandler

  require Logger

  @spec add(AddROTransaction.Request.t(), Stream.t()) ::
          AddROTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node.id, Executor) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    tx_noun = request.transaction.transaction |> Noun.Jam.cue!()

    Executor.launch(
      request.node.id,
      {{:read_only, self()}, tx_noun}
    )

    # todo: GRPC/HTTP requests should not be long-lived. They should be as short
    #       as possible. In the future this would return the tx id, and the
    #       result of the transaction will be sent to the client via an event.
    receive do
      {_time, :error} ->
        %AddROTransaction.Response{result: {:error, %Error{error: "absent"}}}

      {_time, result} ->
        %AddROTransaction.Response{
          result: {:success, %Success{result: result |> Noun.Jam.jam()}}
        }
    end
  end
end
