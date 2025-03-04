defmodule Anoma.Node.Transport.GRPC.Advertise do
  alias Anoma.Node.Transport.NetworkRegister.Advertise
  alias Anoma.Proto.AdvertisementService
  alias Anoma.Proto.Advertisement.Advertise
  alias Anoma.Proto.Advertisement.GRPCAddress
  alias Anoma.Proto.Node

  @behaviour Anoma.Node.Transport.Advertise

  @doc """
  I advertise to a remote node that has advertised to us.
  This means I already know this node, and I am sending it my own advertisement.

  I do this via GRPC only, and I only advertise my GRPC endpoint.
  """
  def advertise(node_config, remote_node_id, remote_node_advert) do
    # remote grpc address
    %{host: remote_host, port: remote_port} = remote_node_advert.grpc_address

    # create our advertisement
    request = %Advertise.Request{
      remote_node: %Node{id: node_config.node_id},
      node: %Node{id: remote_node_id},
      grpc_address: %GRPCAddress{
        host: node_config.instance.node_grpc_host,
        port: node_config.instance.node_grpc_port
      }
    }

    # advertise over grpc
    {:ok, channel} = GRPC.Stub.connect("#{remote_host}:#{remote_port}")

    AdvertisementService.Stub.advertise(channel, request)
    :ok
  end
end
