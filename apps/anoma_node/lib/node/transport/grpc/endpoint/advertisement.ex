defmodule Anoma.Node.Transport.GRPC.Servers.Advertisement do
  @moduledoc """
  Implementation of the GRPC endpoint for node advertisement.
  """

  alias Anoma.Node.Transport.NetworkRegister
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Proto.Advertisement.Advertise

  use GRPC.Server, service: Anoma.Proto.AdvertisementService.Service

  require Logger

  def advertise(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    node_id = request.node.id
    remote_node_id = request.remote_node.id
    grpc_address = request.grpc_address

    # create an advert based on the request
    advert = %NetworkRegister.Advert{
      node_id: remote_node_id,
      version: "unknown",
      grpc_address: %GRPCAddress{
        host: grpc_address.host,
        port: grpc_address.port
      }
    }

    # send the advert to the local network register
    NetworkRegister.node_advert(node_id, remote_node_id, advert)

    # reply ok
    %Advertise.Response{}
  end
end
