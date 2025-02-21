defmodule Anoma.Client.GRPC.PubSub do
  alias Anoma.Proto.PubSub.Event

  use GRPC.Server, service: Anoma.Proto.PubSubService.Service

  require Logger

  alias Phoenix.PubSub

  @doc """
  I handle incoming events sent to me by a node.
  """
  def publish(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    IO.puts("got an event: #{inspect(request)}")
    topic = request.topic.topic
    message = request.message.message

    PubSub.broadcast(
      :client_pubsub,
      "node_events",
      {:event, {topic, message}}
    )

    %Event.Response{}
  end
end
