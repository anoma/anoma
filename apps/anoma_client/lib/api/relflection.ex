defmodule Anoma.Client.Api.ReflectionServer do
  @moduledoc """
  I implement the reflection endpoint for the server.

  I define all the services that have to be available via the reflection endpoint.
  """
  use GrpcReflection.Server,
    version: :v1alpha,
    services: [
      Anoma.Proto.IntentpoolService,
      Anoma.Proto.NockService.Service,
      Anoma.Proto.MempoolService.Service,
      Anoma.Proto.ExecutorService.Service,
      Anoma.Proto.IndexerService.Service
    ]
end
