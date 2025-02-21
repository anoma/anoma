defmodule Anoma.Client.Web.Socket do
  @behaviour Phoenix.Socket.Transport

  alias Phoenix.PubSub

  def child_spec(_opts) do
    # We won't spawn any process, so let's ignore the child spec
    :ignore
  end

  def connect(state) do
    # Callback to retrieve relevant data from the connection.
    # The map contains options, params, transport and endpoint keys.
    {:ok, state}
  end

  def init(state) do
    # Now we are effectively inside the process that maintains the socket.
    PubSub.subscribe(:client_pubsub, "node_events")
    {:ok, state}
  end

  def handle_in({text, _opts}, state) do
    {:reply, :ok, {:text, text}, state}
  end

  @spec handle_info(any(), any()) :: {:push, {:text, String.t()}, any()}
  def handle_info({:event, {_topic, message}}, state) do
    IO.inspect(message, label: "message")
    # the message field is already an encoded json string so i decode it, so I
    # can encode it with the outer topic information
    {:push, {:text, message}, state}
  end

  def terminate(_reason, _state) do
    :ok
  end
end
