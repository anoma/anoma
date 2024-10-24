defmodule Anoma.Client.Api.ReflectionServer do
  alias Anoma.Protobuf.Intents

  use GrpcReflection.Server,
    version: :v1alpha,
    services: [Intents.Service]
end
