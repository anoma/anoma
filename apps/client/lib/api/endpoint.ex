# Define your endpoint
defmodule Client.Api.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Client.Api.Server)
end
