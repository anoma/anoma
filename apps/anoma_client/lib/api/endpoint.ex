# Define your endpoint
defmodule Anoma.Client.Api.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Anoma.Client.Api.Server)
end
