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
  @spec connect(GRPCAddress.t() | TCPAddress.t()) ::
          {:ok, GRPC.Channel.t()} | {:error, term()}
  def connect(%{host: host, port: port}) do
    # One persistent channel, reused for every message (not per-send).
    GRPC.Stub.connect("#{host}:#{port}")
  end

  @impl true
  @spec publish(GRPC.Channel.t(), String.t(), EventBroker.Event.t()) ::
          :ok | {:error, term()}
  def publish(channel, topic, event) do
    request =
      Event.Request.new(%{
        topic: %PubSub.Topic{topic: topic},
        message: %PubSub.Message{message: :erlang.term_to_binary(event)}
      })

    case PubSubService.Stub.publish(channel, request) do
      {:ok, %Event.Response{}} -> :ok
      {:error, _} = err -> err
    end
  end

  @impl true
  @spec call(GRPC.Channel.t(), map()) :: {:ok, term()} | {:error, term()}
  def call(channel, message) do
    request =
      Call.Request.new(%{
        node: %Node{id: message.to},
        from: %Node{id: message.from},
        message: :erlang.term_to_binary(message.message),
        engine: "#{message.engine}"
      })

    case IntraNodeService.Stub.call(channel, request) do
      {:ok, response} ->
        {:ok, response |> Map.get(:message) |> :erlang.binary_to_term()}

      {:error, _} = err ->
        err
    end
  end

  @impl true
  @spec cast(GRPC.Channel.t(), map()) :: :ok | {:error, term()}
  def cast(channel, message) do
    request =
      Cast.Request.new(%{
        node: %Node{id: message.to},
        from: %Node{id: message.from},
        message: :erlang.term_to_binary(message.message),
        engine: "#{message.engine}"
      })

    case IntraNodeService.Stub.cast(channel, request) do
      {:ok, %Cast.Response{}} -> :ok
      {:error, _} = err -> err
    end
  end
end
