defmodule Anoma.LocalDomain.GRPC.Endpoint do
  @moduledoc """
  I am the GRPC endpoint for the client.

  This endpoint is used by nodes to send me events.
  """
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Anoma.LocalDomain.GRPC.PubSub)
end
