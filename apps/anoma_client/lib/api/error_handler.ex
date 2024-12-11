defmodule Anoma.Client.Api.Servers.ErrorInterceptor do
  require Logger

  @behaviour GRPC.Server.Interceptor

  @impl true
  def init(opts) do
    opts
  end

  @impl true
  def call(req, stream, next, opts) do
    IO.inspect(binding())
    res = next.(req, stream)
    IO.puts("result")
    IO.inspect(res)
    res
  end
end
