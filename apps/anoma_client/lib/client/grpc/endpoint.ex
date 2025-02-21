# Define your endpoint
defmodule Anoma.Client.GRPC.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Anoma.Client.GRPC.PubSub)
end
