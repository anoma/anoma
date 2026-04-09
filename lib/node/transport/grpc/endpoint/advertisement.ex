defmodule Anoma.Node.Transport.GRPC.Servers.Advertisement do
  @moduledoc """
  Implementation of the GRPC endpoint for node advertisement.
  """

  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transport.NetworkRegister
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Proto.Advertisement.Advertise
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.AdvertisementService.Service

  require Logger

  import Anoma.Protobuf.ErrorHandler

  @doc """
  This endpoint is called when a node wants to advertise to use via GRPC.

  To handle an advertisement, I pass the advertisement to my network registry
  that will handle it accordingly.
  """
  @spec advertise(Advertise.Request.t(), Stream.t()) :: Advertise.Response.t()
  def advertise(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    # validate the request. will raise if not valid.
    validate_request!(request)

    # ensure the node id exists
    if Registry.whereis(request.node.id, Mempool) == nil do
      raise_grpc_error!(:invalid_node_id)
    end

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
