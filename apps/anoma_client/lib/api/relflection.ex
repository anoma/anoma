defmodule Anoma.Client.Api.ReflectionServer do
  use GrpcReflection.Server,
    version: :v1alpha,
    services: [
      Anoma.Protobuf.IntentsService.Service,
      Anoma.Protobuf.NockService.Service,
      Anoma.Protobuf.ExecutorService.Service
    ]
end
