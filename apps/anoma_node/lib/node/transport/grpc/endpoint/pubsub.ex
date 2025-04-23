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

  def publish(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    # reconstruct the event and fire it on the eventbroker
    event = :erlang.binary_to_term(request.message.message)

    # the incoming event is wrapped in an ExternalEvent wrapper.
    # Remove this wrapper and publish it as a regular event.
    # todo: this is horrible. really really horrible.
    case event do
      %EventBroker.Event{body: %Anoma.Node.Event{body: %Events.External{}}} ->
        # Anoma.Node.Event
        node_event = event.body
        # ExternalEvent
        inner_event = node_event.body
        # the event wrapped in an external event
        actual_event = inner_event.event

        new_event = %{event | body: %{node_event | body: actual_event}}
        EventBroker.event(new_event)

      _ ->
        :noop
    end

    %Event.Response{}
  end
end
