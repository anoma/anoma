defmodule Anoma.Node.Transport.GRPC.Servers.Executor do
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Proto.Executor.RunScry
  alias Anoma.Proto.Nock.Error
  alias Anoma.Proto.Nock.Success
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.ExecutorService.Service

  import Anoma.Protobuf.ErrorHandler

  require Logger

  @spec run_scry(RunScry.Request.t(), Stream.t()) ::
          RunScry.Response.t()
  def run_scry(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node.id, Executor) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

    key_noun = request.key |> Noun.Jam.cue!()

    value =
      Executor.scry(
        request.node.id,
        :read_only,
        key_noun
      )

    case value do
      :error ->
        %RunScry.Response{result: {:error, %Error{error: "absent"}}}

      {:ok, result} ->
        %RunScry.Response{
          result: {:success, %Success{result: result |> Noun.Jam.jam()}}
        }
    end
  end
end
