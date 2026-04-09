defmodule Anoma.Node.Transport.GRPC.Advertise do
  alias Anoma.Node.Transport.NetworkRegister.Advert
  alias Anoma.Node.Transport.NetworkRegister.Advertise
  alias Anoma.Proto.Advertisement.Advertise
  alias Anoma.Proto.Advertisement.GRPCAddress
  alias Anoma.Proto.AdvertisementService
  alias Anoma.Proto.Node

  @behaviour Anoma.Node.Transport.Advertise

  @impl true
  @doc """
  I listen for advertisements from other nodes.

  Any request will hold the information of another node and how to contact it.
  When I receive such a request, I reply with my own advertisement.
  """
  def advertise(_, _, %Advert{grpc_address: nil}), do: :ok

  def advertise(node_config, remote_node_id, remote_node_advert) do
    # remote grpc address
    %{host: remote_host, port: remote_port} = remote_node_advert.grpc_address

    # create our advertisement
    request = %Advertise.Request{
      remote_node: %Node{id: node_config.node_id},
      node: %Node{id: remote_node_id},
      grpc_address: %GRPCAddress{
        host: node_config.grpc_host,
        port: node_config.grpc_port
      }
    }

    # advertise over grpc
    {:ok, channel} = GRPC.Stub.connect("#{remote_host}:#{remote_port}")

    AdvertisementService.Stub.advertise(channel, request)
    :ok
  end
end
