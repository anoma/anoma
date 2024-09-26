defmodule Anoma.Node.Transport.EngineProxy do
  @moduledoc """
  I am an engine proxy and I serve as the single point of contact to a remote node.

  I handle the outgoing messages for the node I proxy.
  For any given message I receive, I look for a connection to the remote node to send the message.
  These connections can be TCP connection, websockets, or any other connection type.

  The incoming messages are sent to the right engine directly by the connection.
  """
  use GenServer
  use TypedStruct

  require Logger

  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport.Messages.Handler
  alias Anoma.Node.Transport.Registry
  alias EventBroker.Event
  alias EventBroker.Filters

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of the proxy engine.

    My fields contain information to facilitate the connection with a remote engine.

    ### Fields
     - `:remote_id`      - the id of the remote engine.
     - `:type`           - the type of the connection (e.g., :router, :mempool, ..)
     - `:message_queue`  - A queue of messages for the remote engine.
    """
    field(:node_id, Id.t())
    field(:remote_node_id, Id.t())
    field(:type, atom())
    field(:async_queue, [term()], default: [])
    field(:sync_queue, [term()], default: [])
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :remote_node_id, :type])

    # register as a type of proxy engine for a remote node, at the local registry.
    # e.g., {TCP.Server, remote_node_id}
    name =
      Registry.key(
        args[:node_id],
        args[:remote_node_id],
        args[:type]
      )

    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @doc """
  I am the init function for an engine proxy.
  I register this process as a proxy engine in the registry for the given remote node id.
  """
  @impl true
  def init(args) do
    Logger.debug("starting engine proxy with #{inspect(args)}")

    # trap exits to shut down cleanly.
    Process.flag(:trap_exit, true)

    args = Keyword.validate!(args, [:node_id, :remote_node_id, :type])
    state = struct(__MODULE__, Enum.into(args, %{}))

    # subscribe to discovery events
    EventBroker.subscribe_me([%Filters.SourceModule{module: Handler}])

    {:ok, state}
  end

  # ----------------------------------------------------------------------------
  # Casts

  @impl true
  # @doc """
  # I receive this message to forward it to the remote node via an available connection.
  # """
  def handle_cast({:asynchronous_message, message}, state) do
    Logger.debug("forwarding async message: #{inspect(message)}")

    node_id = state.node_id
    remote_node_id = state.remote_node_id
    type = state.type

    case handle_async_message(message, node_id, remote_node_id, type) do
      # message sent
      {:ok, :sent} ->
        {:noreply, state}

      # queue the message
      {:error, :no_connection} ->
        new_state =
          Map.update(state, :async_queue, [message], &[message | &1])

        {:noreply, new_state}
    end
  end

  # ----------------------------------------------------------------------------
  # Calls

  @impl true
  # @doc """
  # I receive this message to forward it to the remote node via an available connection.
  # """
  def handle_call({:synchronous_message, message, expiration}, from, state) do
    Logger.debug("forwarding sync message: #{inspect(message)}")

    node_id = state.node_id
    remote_node_id = state.remote_node_id
    type = state.type

    case handle_sync_message(message, node_id, remote_node_id, type) do
      # message sent
      {:ok, :sent} ->
        {:reply, :ok, state}

      # no connection available, queue the message
      {:error, :no_connection} ->
        message_ref = make_ref()
        to_queue = {message, expiration, from, message_ref}

        new_state =
          Map.update(state, :sync_queue, [to_queue], &[to_queue | &1])

        # remove message after expiration
        timeout = max(0, expiration - System.monotonic_time(:millisecond))
        Process.send_after(self(), {:check_timeout, message_ref}, timeout)

        {:noreply, new_state}
    end
  end

  # ----------------------------------------------------------------------------
  # Infos

  @impl true
  # @doc """
  # I receive this message when I have to remove a message from the queue
  # because it expired. The message could have been sent in the mean while.
  # """
  def handle_info({:check_timeout, message_ref}, state) do
    state =
      Map.update(state, :sync_queue, [], fn queue ->
        Enum.reject(queue, fn
          {_, _, _, ref} -> ref == message_ref
        end)
      end)

    {:noreply, state}
  end

  # @doc """
  # I receive this event when a new node has been discovered.
  # """
  def handle_info(%Event{body: :new_node_discovered}, state) do
    new_state = handle_new_proxy(state)

    {:noreply, new_state}
  end

  def handle_info(%Event{}, state) do
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # @doc """
  # I handle the event of a new proxy engine starting up.
  # This event means I can potentially flush some messages out of my queue.
  # """
  @spec handle_new_proxy(t()) :: t()
  defp handle_new_proxy(state) do
    node_id = state.node_id
    remote_node_id = state.remote_node_id
    type = state.type

    # flush out the async queue
    async_queue =
      flush_async_queue(state.async_queue, node_id, remote_node_id, type)

    sync_queue =
      flush_sync_queue(state.sync_queue, node_id, remote_node_id, type)

    %{state | async_queue: async_queue, sync_queue: sync_queue}
  end

  # @doc """
  # I handle an incoming asynchronous message.
  # I try to send the message if there is an active connection.
  # """
  @spec handle_async_message(term(), Id.t(), Id.t(), atom()) ::
          {:ok, :sent} | {:error, :no_connection}
  defp handle_async_message(message, local_node_id, remote_node_id, type) do
    # lookup the connection for the remote node (e.g., tcp)
    case Registry.lookup_by_label(local_node_id, remote_node_id, :transport) do
      [{_, _, :transport, pid, _}] ->
        message = %{to: type, message: message, type: :async}

        GenServer.cast(pid, {:send, message})

        {:ok, :sent}

      [] ->
        {:error, :no_connection}
    end
  end

  # @doc """
  # I try to send a synchronous message to a remote node over an available connection.
  # If there is no connection I return an error.
  # """
  @spec handle_sync_message(term(), Id.t(), Id.t(), atom()) ::
          {:ok, :sent} | {:error, :no_connection}
  defp handle_sync_message(message, local_node_id, remote_node_id, type) do
    # lookup the connection for the remote node (e.g., tcp)
    case Registry.lookup_by_label(local_node_id, remote_node_id, :transport) do
      [{_, _, :transport, pid, _}] ->
        message = %{to: type, message: message, type: :sync}

        GenServer.cast(pid, {:send, message})

        {:ok, :sent}

      [] ->
        {:error, :no_connection}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I flush the async queue by trying to send all the messages in there.
  # I return the new queue.
  # """
  @spec flush_async_queue([term()], Id.t(), Id.t(), atom()) :: [term()]
  defp flush_async_queue(queue, node_id, remote_node_id, type) do
    Enum.reject(queue, fn message ->
      case handle_async_message(message, node_id, remote_node_id, type) do
        {:ok, :sent} ->
          true

        {:error, :no_connection} ->
          false
      end
    end)
  end

  # @doc """
  # I flush the sync queue by trying to send all the messages in there.
  # I return the new queue.
  # """
  @spec flush_sync_queue([term()], Id.t(), Id.t(), atom()) :: [term()]
  defp flush_sync_queue(queue, node_id, remote_node_id, type) do
    Enum.reject(queue, fn {message, _expiration, from, _message_ref} ->
      case handle_sync_message(message, node_id, remote_node_id, type) do
        {:ok, :sent} ->
          GenServer.reply(from, :ok)
          true

        {:error, :no_connection} ->
          false
      end
    end)
  end
end
