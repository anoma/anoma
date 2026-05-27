defmodule Anoma.Node.Transport.GRPC.Servers.PubSub do
  alias Anoma.Node.Transport.Proxy
  alias Anoma.Node.Transport.Proxy.Events
  alias Anoma.Proto.PubSub.Event
  alias Anoma.Proto.PubSub.Subscribe
  alias Anoma.Proto.PubSub.Unsubscribe
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Proto.PubSubService.Service

  import Anoma.Protobuf.ErrorHandler

  require Logger

  @spec subscribe(Subscribe.Request.t(), Stream.t()) :: Subscribe.Response.t()
  def subscribe(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    remote_node_id = request.subscriber.id

    # tell the node proxy to subscribe
    case Proxy.Node.subscribe(remote_node_id, request.topic.topic) do
      :ok ->
        %Subscribe.Response{success: true}

      {:error, :invalid_topic} ->
        raise_grpc_error!(:invalid_topic)

      {:error, :could_not_subscribe, err} ->
        raise_grpc_error!(err)
    end
  end

  @spec unsubscribe(Unsubscribe.Request.t(), Stream.t()) ::
          Unsubscribe.Response.t()
  def unsubscribe(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    %Unsubscribe.Response{}
  end

  @spec publish(Anoma.Proto.PubSub.Event.Request.t(), Stream.t()) ::
          Event.Response.t()
  def publish(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    # reconstruct the event and fire it on the eventbroker
    event = :erlang.binary_to_term(request.message.message)

    # The incoming event is wrapped in an Events.External wrapper; strip
    # it and republish the inner event as a regular local event.
    case event do
      %EventBroker.Event{
        body:
          %Anoma.Node.Event{body: %Events.External{event: inner}} = node_event
      } ->
        EventBroker.event(%{event | body: %{node_event | body: inner}})

      _ ->
        :noop
    end

    %Event.Response{}
  end
end
