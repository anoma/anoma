# Define your endpoint
defmodule Anoma.Client.Api.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Anoma.Client.Api.Servers.Intents)
  run(Anoma.Client.Api.Servers.Nock)
  run(Anoma.Client.Api.Servers.Mempool)
  run(Anoma.Client.Api.Servers.Executor)
  run(Anoma.Client.Api.ReflectionServer)
end
