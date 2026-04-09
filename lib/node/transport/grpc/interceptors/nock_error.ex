defmodule GRPC.Server.Interceptors.NockErrors do
  @moduledoc """
  I catch specific Nock encoding errors, and turn them into a generic `GRPC.RPCError` instance.
  """
  alias Anoma.Protobuf.ErrorHandler

  @behaviour GRPC.Server.Interceptor

  @impl true
  def init(opts) do
    opts
  end

  @impl true
  def call(req, stream, next, _opts) do
    try do
      next.(req, stream)
    rescue
      e in Noun.Jam.CueError ->
        ErrorHandler.raise_grpc_error!(e)
    end
  end
end
