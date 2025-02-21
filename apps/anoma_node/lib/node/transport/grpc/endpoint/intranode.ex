defmodule Anoma.Node.Transport.GRPC.Servers.IntraNode do
  use GRPC.Server, service: Anoma.Proto.IntraNodeService.Service

  require Logger
end
