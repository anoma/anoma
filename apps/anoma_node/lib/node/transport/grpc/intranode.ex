defmodule Anoma.Node.Transport.GRPC.Behavior do
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister.Advert.TCPAddress
  alias Anoma.Proto.IntraNode.Call
  alias Anoma.Proto.IntraNodeService
  alias Anoma.Proto.Node
  alias Anoma.Proto.PubSub
  alias Anoma.Proto.PubSubService

  alias Anoma.Proto.PubSubService

  @spec call(GRPCAddress.t() | TCPAddress.t(), map()) :: {:ok, String.t()}
  def call(address, message) do
    %{host: host, port: port} = address

    # connect to the grpc endpoint
    {:ok, channel} = GRPC.Stub.connect("#{host}:#{port}")

    to_node_id = message.to
    payload = message.message
    engine = message.engine

    request =
      Call.Request.new(%{
        node: %Node{id: to_node_id},
        message: inspect(payload),
        engine: "#{engine}"
      })

    {:ok, response} = IntraNodeService.Stub.call(channel, request)
    Map.get(response, :message)
  end

  def event(address, event) do
    %{host: host, port: port} = address

    # connect to the grpc endpoint
    {:ok, channel} = GRPC.Stub.connect("#{host}:#{port}")

    request =
      PubSub.Event.Request.new(%{
        topic: %PubSub.Topic{topic: event.topic},
        message: %PubSub.Message{message: inspect(event.event)}
      })

    IO.inspect(request)

    PubSubService.Stub.publish(channel, request)
  end
end
