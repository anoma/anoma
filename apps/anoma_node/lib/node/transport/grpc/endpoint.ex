# Define your endpoint
defmodule Anoma.Node.Transport.GRPC.Endpoint do
  @moduledoc """
  I define all the endpoints that are going to be available via the GRPC endpoint.

  Each endpoint listed below implements a server defined in the protobuf file.

  For example, the `anoma.proto` file defines a couple of services:
    - `Intents`
    - `Indexer`
    - `Nock`
    - `Mempool`

  Each of these services have an implementation in Elixir in the module `Anoma.Client.Api.Servers.*`.
  """
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.Logger)
  run(Anoma.Node.Transport.GRPC.Servers.Intents)
  run(Anoma.Node.Transport.GRPC.Servers.Indexer)
  run(Anoma.Node.Transport.GRPC.Servers.Mempool)
  run(Anoma.Node.Transport.GRPC.Servers.Executor)
  run(Anoma.Node.Transport.GRPC.Servers.Blocks)
end
