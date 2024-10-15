# Define your endpoint
defmodule Anoma.Client.Api.Endpoint do
  alias Anoma.Client

  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Client.Api.Server)
end
