defmodule Anoma.Client.GRPC.PubSub do
  @moduledoc """
  I handle all incoming request for the PubSub endpoint of the client.
  """

  alias Anoma.Proto.PubSub.Event
  alias GRPC.Server.Stream
  alias Phoenix.PubSub

  use GRPC.Server, service: Anoma.Proto.PubSubService.Service

  require Logger

  @doc """
  I handle incoming events sent t`o me by a node.
  """
  @spec publish(Event.Request.t(), Stream.t()) :: Event.Response.t()
  def publish(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    topic = request.topic.topic
    event = :erlang.binary_to_term(request.message.message)

    PubSub.broadcast(
      :client_pubsub,
      "node_events",
      {:event, {topic, event}}
    )

    %Event.Response{}
  end
end
