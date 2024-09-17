defmodule Anoma.Node.Transport2.EngineProxy do
  @moduledoc """
  I am an engine proxy and I serve as the single point of contact to a remote node.
  """
  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Crypto.Id

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
    field(:message_queue, [term()], default: [])
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
    IO.puts("Starting a proxy engine for #{inspect(state.remote_id)}")
    {:ok, _} = register_proxy(state)

    {:ok, state}
  end

  # @doc """
  # I send an asynchronous message to the remote engine via
  # an available connection.
  # """
  @impl true
  def handle_cast({:send_async, message}, state) do
    {:ok, _, new_state} = handle_async_send(message, state)
    {:noreply, new_state}
  end

  # @doc """
  # I am a message signaling that a new network connection is available
  # to my remote node.
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
    {:ok, _, new_state} = handle_sync_send(message, timeout, from, state)
    {:noreply, new_state}
  end

  # @doc """
  # I check if there is a message in my queue with the given inbox.
  # If there is, it has expired and I reply an error to the original sender.
  # """
  @impl true
  def handle_info({:check_timeout, message_ref}, state) do
    {:ok, _, new_state} = handle_check_timeout(message_ref, state)
    {:noreply, new_state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_new_connection(t(), pid()) :: t()
  defp handle_new_connection(state, _pid) do
    state.message_queue
    |> Enum.each(fn
      %{type: :async, message: m} ->
        GenServer.cast(self(), {:send_async, m})

      %{type: :sync, message: m, from: from} ->
        handle_sync_send(m, 1000, from, state)
    end)

    %{state | message_queue: []}
  end

  # @doc """
  # I send an async message to the given remote id.
  # I lookup the available connection types and send the message
  # to the first available connection.
  # """
  @spec handle_async_send(Id.t(), any()) :: {:ok, :queued | :sent, t()}
  defp handle_async_send(message, state) do
    case lookup_connection(state, [:tcp]) do
      [] ->
        IO.inspect("queueing message")
        queued_message = %{type: :async, message: message}
        {:ok, state} = queue_message(queued_message, state)
        IO.inspect(state)
        {:ok, :queued, state}

      [{_type, _id, pid} | _] ->
        IO.inspect("sending message")

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
  @spec handle_sync_send(any(), integer(), pid(), Id.t()) ::
          {:ok, :sent} | {:error, :no_connection} | {:ok, :queued}
  defp handle_sync_send(message, timeout, from, state) do
    case lookup_connection(state, [:tcp]) do
      [] ->
        message_ref = make_ref()

        queued_message = %{
          type: :sync,
          message: message,
          from: from,
          ref: message_ref
        }

        {:ok, state} = queue_message(queued_message, state)
        Process.send_after(self(), {:check_timeout, message_ref}, timeout)
        {:ok, :queued, state}

      [{_type, _id, pid} | _] ->
        # note: we could just send the message to {:via..} here, but there will be different
        #       types of connections, so a lookup is waranted here.
        GenServer.cast(pid, {:send, message})
        GenServer.reply(from, {:ok, :sent})
        {:ok, :sent, state}
    end
  end

  # @doc """
  # I check if there is a message in my queue with the given reference.
  # If the message is there, the timeout has expired and I notify the sender of a failure.
  # If the message is not there, the message was sent sucessfully.
  # """
  defp handle_check_timeout(message_ref, state) do
    {message, queue} =
      state.message_queue
      |> Enum.split_with(fn
        %{type: :sync, ref: ^message_ref} -> true
        _ -> false
      end)

    new_state = %{state | message_queue: queue}

    case message do
      [] ->
        {:ok, :not_queued, new_state}

      [m] ->
        {:ok, :removed, new_state}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I queue a message to be sent to the remote engine.
  # If the engine is not present right now, the queue builds up.
  # """
  @spec queue_message(term(), t()) :: {:ok, t()}
  defp queue_message(message, state) do
    {:ok, Map.update(state, :message_queue, [message], &[message | &1])}
  end

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
  # I lookup any possible connections that can be used to transmit data to a remote node.
  # I lookup all connections for a given id of type :tcp or :tor.
  # """
  @spec lookup_connection(t(), [atom()]) :: [{:tcp | :tor, Id.t(), pid()}]
  def lookup_connection(state, types \\ []) do
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
