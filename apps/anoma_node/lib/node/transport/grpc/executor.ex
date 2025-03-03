defmodule Anoma.Node.Transport.GRPC.Servers.Executor do
  alias Anoma.Protobuf.Executor.AddROTransaction
  alias GRPC.Server.Stream
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Protobuf.Nock.Success
  alias Anoma.Protobuf.Nock.Error

  use GRPC.Server, service: Anoma.Protobuf.ExecutorService.Service

  require Logger

  @spec add(AddROTransaction.Request.t(), Stream.t()) ::
          AddROTransaction.Response.t()
  def add(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    tx_noun = request.transaction |> Noun.Jam.cue!()

    Executor.launch(
      request.node_info.node_id,
      {{:read_only, self()}, tx_noun}
    )

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
