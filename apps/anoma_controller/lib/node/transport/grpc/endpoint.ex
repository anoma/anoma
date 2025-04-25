# Define your endpoint
defmodule Anoma.Node.Transport.GRPC.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Anoma.Node.Transport.GRPC.Servers.Intents)
  run(Anoma.Node.Transport.GRPC.Servers.Mempool)
  run(Anoma.Node.Transport.GRPC.Servers.Executor)
end
