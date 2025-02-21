defmodule Anoma.Client.Api.ReflectionServer do
  use GrpcReflection.Server,
    version: :v1alpha,
    services: [
      Anoma.Proto.IntentpoolService,
      Anoma.Proto.NockService.Service
    ]
end
