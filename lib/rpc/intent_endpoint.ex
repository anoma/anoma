defmodule RPC.IntentEndpoint do
  @moduledoc """
  I am an HTTP endpoint for gRPC requests.
  """

  use GRPC.Endpoint
  run(RPC.IntentPool)
end
