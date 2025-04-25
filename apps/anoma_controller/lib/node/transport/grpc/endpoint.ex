# Define your endpoint
defmodule Anoma.Controller.Transport.GRPC.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Anoma.Controller.Transport.GRPC.Servers.Intents)
  run(Anoma.Controller.Transport.GRPC.Servers.Mempool)
  run(Anoma.Controller.Transport.GRPC.Servers.Executor)
end
