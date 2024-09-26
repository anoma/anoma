defmodule Anoma.Node.Transport.Messages.Handler do
  @moduledoc """
  I contain logic on how to handle the different types of messages that come in over the network.

  ### Public API

  I provide the following public functionality:

  - `handle_message/3`
  """
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport.Discovery
  alias Anoma.Node.Transport.Registry
  alias EventBroker.Event

  require Logger

  @doc """
  I handle a new message that came in over the network.

  ### Options

  - `:connection_type` - This flag sets the type of connection over which the message came in.
                         I am used to register a connection in the registry.

  ### Pattern-Matching Variations

  - `init(%{type: :inform}, _, _)`
     I handle a message coming from a remote node with its information in it.

  - `init(%{to: _, type: :async}, _, _)`
    I handle an asynchronous message.

  - `init(%{to: _, type: :sync}, _, _)`
    I handle an synchronous message.

  - `init(_, _, _)`
    I handle invalid messages.
  """
  @spec handle_message(term(), Id.t(), Keyword.t()) :: :ok
  def handle_message(message, node_id, opts \\ [])

  # @doc """
  # I handle discovery messages from other nodes.
  # """
  def handle_message(message = %{type: :inform}, node_id, opts) do
    opts = Keyword.validate!(opts, [:connection_type])
    connection_type = Keyword.fetch!(opts, :connection_type)
    Logger.debug("discovered a new node #{inspect(message)}")
    remote_node_id = message.node_id

    # register the current process as the connection for the remote node
    Registry.register(
      node_id,
      message.node_id,
      connection_type,
      :transport
    )

    # create a proxy for the node
    Discovery.create_node_proxy(remote_node_id, node_id)

    # send an event that a new node was discovered
    %Event{source_module: __MODULE__, body: :new_node_discovered}
    |> EventBroker.event()

    :ok
  end

  # @doc """
  # I handle asynchronous messages to be forwarded to the local engine.
  # """
  def handle_message(m = %{to: _, message: _, type: :async}, local_node_id, _) do
    Logger.debug("received message for engine: #{inspect(m.to)}")
    Logger.debug("message: #{inspect(m)}")

    case Registry.lookup(local_node_id, local_node_id, m.to) do
      [{_, _, _, pid, _}] ->
        GenServer.cast(pid, {:asynchronous_message, m.message})

      [] ->
        Logger.warning("no local engine found for message: #{inspect(m)}")
    end

    :ok
  end

  # @doc """
  # I handle synchronous messages to be forwarded to the local engine.
  # """
  def handle_message(m = %{to: _, message: _, type: :sync}, local_node_id, _) do
    Logger.debug("received message for engine: #{inspect(m.to)}")
    Logger.debug("message: #{inspect(m)}")

    case Registry.lookup(local_node_id, local_node_id, m.to) do
      [{_, _, _, pid, _}] ->
        GenServer.call(pid, {:synchronous_message, m.message, 0})

      [] ->
        Logger.warning("no local engine found for message: #{inspect(m)}")
    end

    :ok
  end

  # @doc """
  # I handle invalid messages that are going to be ignored.
  # """
  def handle_message(m, _node_id, _opts) do
    Logger.warning("invalid message received on tcp server: #{inspect(m)}")
    :ok
  end
end
