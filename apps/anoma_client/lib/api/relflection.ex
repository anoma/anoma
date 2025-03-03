defmodule Anoma.Client.Api.ReflectionServer do
  @moduledoc """
  I implement the reflection endpoint for the server.

  I define all the services that have to be available via the reflection endpoint.
  """
  use GrpcReflection.Server,
    version: :v1alpha,
    services: [
      Anoma.Protobuf.IntentsService.Service,
      Anoma.Protobuf.IndexerService.Service,
      Anoma.Protobuf.BlockService.Service,
      Anoma.Protobuf.MempoolService.Service,
      Anoma.Protobuf.NockService.Service,
      Anoma.Protobuf.ExecutorService.Service
    ]
end
