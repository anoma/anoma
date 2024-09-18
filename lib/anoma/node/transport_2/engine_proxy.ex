defmodule Anoma.Node.Transport2.EngineProxy do
  @moduledoc """
  I am an engine proxy and I serve as the single point of contact to a remote node.
  """
  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport2.EngineProxy.AsyncMessage
  alias Anoma.Node.Transport2.EngineProxy.SyncMessage

  ############################################################
  #                    Messages                              #
  ############################################################

  typedstruct module: SyncMessage do
    @typedoc """
    I contain all data for a synchronous message send.

    My fields contain information to keep track of the message and to send it to the remote.

    ### Fields
    - `:from`          - The process id of the sender.
    - `:message`       - The message to send.
    - `:timeout`       - The timeout of the message.
    - `:ref`           - The unique reference of the message.
    - `:timestamp`     - The timestamp of the message.
    """
    field(:from, term())
    field(:message, map())
    field(:timeout, integer())
    field(:ref, reference())
    field(:timestamp, integer())
  end

  typedstruct module: AsyncMessage do
    @typedoc """
    I contain all data for an asynchronous message send.

    My fields contain information to keep track of the message and to send it to the remote.

    ### Fields
    - `:message`       - The message to send.
    """
    field(:message, map())
  end

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of the proxy engine.

    My fields contain information to facilitate the connection with a remote engine.

    ### Fields
     - `:remote_id`      - the id of the remote engine.
      - `:type`          - the type of the connection (e.g., :router, :mempool, ..)
      - `:message_queue` - A queue of messages for the remote engine.
    """
    field(:remote_id, Id.t())
    field(:type, atom())
    field(:message_queue, [SyncMessage.t() | AsyncMessage.t()], default: [])
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
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
  def init([remote_id]) do
    state = %EngineProxy{remote_id: remote_id}
    {:ok, _} = register_proxy(state)

    {:ok, state}
  end

  # @doc """
  # I send an asynchronous message to the remote engine via
  # an available connection.
  # """
  @impl true
  def handle_cast({:send_async, message}, state) do
    message = %AsyncMessage{message: message}
    {:ok, _, new_state} = handle_async_send(message, state)
    {:noreply, new_state}
  end

  # @doc """
  # I am a message signaling that a new network connection is available
  # to my node.
  # If I have messages queued, I will flush them.
  # """
  @impl true
  def handle_cast({:new_connection, pid}, state) do
    state = handle_new_connection(state, pid)
    {:noreply, state}
  end

  # @doc """
  # I send a synchronous message to the remote engine via
  # an available connection.
  # """
  @impl true
  def handle_call({:send_sync, message, timeout}, from, state) do
    message = %SyncMessage{
      message: message,
      from: from,
      timeout: timeout,
      timestamp: System.monotonic_time(:millisecond),
      ref: make_ref()
    }

    {:ok, _, new_state} = handle_sync_send(message, state)
    {:noreply, new_state}
  end

  # @doc """
  # I check if there is a message in my queue with the given inbox.
  # I do not reply to the original sender, because the timeout had to be specified in the
  # GenServer.call/2 call too, and that will expire on its own.
  # """
  @impl true
  def handle_info({:check_timeout, message_ref}, state) do
    new_state = handle_check_timeout(message_ref, state)
    {:noreply, new_state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # @doc """
  # I send an async message to the given remote id.
  # I lookup the available connection types and send the message
  # to the first available connection.
  # """
  @spec handle_async_send(AsyncMessage.t(), t()) ::
          {:ok, :queued | :sent, t()}
  defp handle_async_send(message, state) do
    case lookup_connection(state, [:tcp]) do
      # no connection available, queue the message
      [] ->
        {:ok, state} = queue_message(message, state)
        {:ok, :queued, state}

      # a connection is found, send the message
      [{_type, _id, pid} | _] ->
        # note: we could just send the message to {:via..} here, but there will be different
        #       types of connections, so a lookup is waranted here.
        GenServer.cast(pid, {:send, message})
        {:ok, :sent, state}
    end
  end

  # @doc """
  # I send a sync message to the given remote id.
  # I lookup the available connection types and send the message
  # to the first available connection.
  # If I cannot send the message, I queue it.
  # """
  @spec handle_sync_send(SyncMessage.t(), t()) :: {:ok, :sent | :queued, t()}
  defp handle_sync_send(message, state) do
    case lookup_connection(state, [:tcp]) do
      [] ->
        {:ok, state} = queue_message(message, state)

        Process.send_after(
          self(),
          {:check_timeout, message.ref},
          message.timeout
        )

        {:ok, :queued, state}

      [{_type, _id, pid} | _] ->
        # note: we could just send the message to {:via..} here, but there will be different
        #       types of connections, so a lookup is waranted here.
        GenServer.cast(pid, {:send, message})

        # reply to the original sender to ack the sending
        GenServer.reply(message.from, {:ok, :sent})
        {:ok, :sent, state}
    end
  end

  # @doc """
  # I flush the queue whenever a new connection appears.
  # If the messages fail, theyre put back in the queue.
  # """
  @spec handle_new_connection(t(), pid()) :: t()
  defp handle_new_connection(state, _pid) do
    flush_queue(state)
  end

  # @doc """
  # I check the queue for the given message and remove it.
  # If the message has been sent it's not in there.
  # If the message expired, it must be removed from the queue.
  # """
  @spec handle_check_timeout(reference(), t()) :: t()
  defp handle_check_timeout(message_ref, state) do
    # fetch the message with the given reference.
    queue =
      state.message_queue
      |> Enum.reject(fn
        %SyncMessage{ref: ^message_ref} -> true
        _ -> false
      end)

    %{state | message_queue: queue}
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I register the current process as the proxy engine for the given remote node id.
  # Currently the type is always :router.
  # """
  @spec register_proxy(t()) :: {:ok, :registered} | {:ok, :already_registered}
  defp register_proxy(state) do
    engine_key = %{remote_id: state.remote_id, type: :router}
    # what could be we store as a value? its optional
    # but I imagine something like connection type or something
    # these values can help in lookups
    engine_value = %{}

    case Registry.register(ProxyRegister, engine_key, engine_value) do
      {:ok, _} -> {:ok, :registered}
      {:error, {:already_registered, _}} -> {:ok, :already_registered}
    end
  end

  # @doc """
  # I flush the queue of messages to the remote engine.
  # """
  @spec flush_queue(t()) :: t()
  defp flush_queue(state) do
    new_state = Map.put(state, :message_queue, [])

    state.message_queue
    |> Enum.reduce(new_state, fn
      %AsyncMessage{} = m, state ->
        {:ok, _, state} = handle_async_send(m, state)
        state

      %SyncMessage{} = m, state ->
        # recompute the timeout
        new_timeout =
          m.timestamp + m.timeout - System.monotonic_time(:millisecond)

        m = %{m | timeout: new_timeout}

        {:ok, _, state} = handle_sync_send(m, state)
        state
    end)
  end

  # @doc """
  # I queue a message to be sent to the remote engine.
  # If the engine is not present right now, the queue builds up.
  # """
  @spec queue_message(term(), t()) :: {:ok, t()}
  defp queue_message(message, state) do
    {:ok, Map.update(state, :message_queue, [message], &[message | &1])}
  end

  # @doc """
  # I lookup any possible connections that can be used to transmit data to a remote node.
  # I lookup all connections for a given id of type :tcp or :tor.
  # """
  @spec lookup_connection(t(), [atom()]) :: [{:tcp | :tor, Id.t(), pid()}]
  defp lookup_connection(state, types) do
    # match pattern: this pattern should match the values in the registry
    # at least {:"$1", :"$2", :"$3"} ({key, pid, value})
    pattern = {%{type: :"$1", remote_id: :"$2"}, :"$3", :"$4"}

    # guards: filters applied on the results
    # the below line filters out specific ids and types
    type_guard =
      Enum.reduce(types, [], fn type, acc ->
        [{:orelse, {:"=:=", :"$1", type}, acc}]
      end)

    guards = [{:==, :"$2", state.remote_id} | type_guard]

    # shape: the shape of the results the registry should return
    shape = [{{:"$1", :"$2", :"$3"}}]
    Registry.select(ProxyRegister, [{pattern, guards, shape}])
  end
end
