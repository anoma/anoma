defmodule Anoma.Node.Transport.GRPC.Servers.PubSub do
  alias Anoma.Proto.PubSub.Subscribe
  alias Anoma.Proto.PubSub.Unsubscribe
  alias Anoma.Proto.PubSub.Event
  alias GRPC.Server.Stream
  alias Anoma.Node.Transport.Proxy

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
    IO.puts("got an event: #{inspect(request)}")
    %Event.Response{}
  end
end
