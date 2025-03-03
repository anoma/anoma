defmodule Anoma.Client.Api.Endpoint do
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
  run(Anoma.Client.Api.Servers.Intents)
  run(Anoma.Client.Api.Servers.Indexer)
  run(Anoma.Client.Api.Servers.Nock)
  run(Anoma.Client.Api.Servers.Mempool)
  run(Anoma.Client.Api.Servers.Blocks)
  run(Anoma.Client.Api.Servers.Executor)
  run(Anoma.Client.Api.ReflectionServer)
end
