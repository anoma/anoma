defmodule Anoma.Node.Transport.GRPC.Behavior do
  alias Anoma.Node.Transport.IntraNode
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister.Advert.TCPAddress
  alias Anoma.Proto.IntraNode.Call
  alias Anoma.Proto.IntraNode.Cast
  alias Anoma.Proto.IntraNodeService
  alias Anoma.Proto.Node
  alias Anoma.Proto.PubSub
  alias Anoma.Proto.PubSub.Event
  alias Anoma.Proto.PubSubService

  @behaviour IntraNode

  @impl true
  @spec publish(
          GRPCAddress.t() | TCPAddress.t(),
          String.t(),
          EventBroker.Event.t()
        ) :: :ok
  def publish(address, topic, event) do
    %{host: host, port: port} = address

    # connect to the grpc endpoint
    {:ok, channel} = GRPC.Stub.connect("#{host}:#{port}")

    request =
      Event.Request.new(%{
        topic: %PubSub.Topic{topic: topic},
        message: %PubSub.Message{message: :erlang.term_to_binary(event)}
      })

    {:ok, %Event.Response{}} = PubSubService.Stub.publish(channel, request)
    :ok
  end

  @impl true
  @spec call(GRPCAddress.t() | TCPAddress.t(), map()) :: {:ok, String.t()}
  def call(address, message) do
    %{host: host, port: port} = address

    # connect to the grpc endpoint
    {:ok, channel} = GRPC.Stub.connect("#{host}:#{port}")

    to_node_id = message.to
    from_node_id = message.from
    payload = message.message
    engine = message.engine

    request =
      Call.Request.new(%{
        node: %Node{id: to_node_id},
        from: %Node{id: from_node_id},
        message: :erlang.term_to_binary(payload),
        engine: "#{engine}"
      })

    {:ok, response} = IntraNodeService.Stub.call(channel, request)

    response =
      response
      |> Map.get(:message)
      |> :erlang.binary_to_term()

    {:ok, response}
  end

  @impl true
  @spec cast(GRPCAddress.t() | TCPAddress.t(), map()) :: :ok
  def cast(address, message) do
    %{host: host, port: port} = address

    # connect to the grpc endpoint
    {:ok, channel} = GRPC.Stub.connect("#{host}:#{port}")

    to_node_id = message.to
    from_node_id = message.from
    payload = message.message
    engine = message.engine

    request =
      Cast.Request.new(%{
        node: %Node{id: to_node_id},
        from: %Node{id: from_node_id},
        message: :erlang.term_to_binary(payload),
        engine: "#{engine}"
      })

    {:ok, %Cast.Response{}} = IntraNodeService.Stub.cast(channel, request)
    :ok
  end
end
