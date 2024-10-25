defmodule Anoma.Node.Transaction.Ordering do
  @moduledoc """
  abordering genserver
  """

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node
  alias Node.Transaction.Storage
  alias Node.Registry

  require Node.Event

  ############################################################
  #                         State                            #
  ############################################################

  @typep startup_options() :: {:node_id, String.t()}

  typedstruct enforce: true do
    field(:node_id, String.t())
    field(:next_height, integer(), default: 1)
    # maps tx ids to their height for writing.
    # the previous height is used for reading.
    field(:tx_id_to_height, %{binary() => integer()}, default: %{})
  end

  typedstruct enforce: true, module: OrderEvent do
    field(:tx_id, binary())
  end

  deffilter TxIdFilter, tx_id: binary() do
    %EventBroker.Event{body: %Node.Event{body: %{tx_id: ^tx_id}}} -> true
    _ -> false
  end

  @spec start_link([startup_options()]) :: GenServer.on_start()
  def start_link(args \\ []) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @spec init([startup_options()]) :: {:ok, t()}
  def init(args) do
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, [:node_id, next_height: 1])
    state = struct(Ordering, Enum.into(args, %{}))
    {:ok, state}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec read(String.t(), {binary(), any()}) :: any()
  def read(node_id, {id, key}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:read, {id, key}},
      :infinity
    )
  end

  @spec write(String.t(), {binary(), list({any(), any()})}) :: :ok
  def write(node_id, {id, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:write, {id, kvlist}},
      :infinity
    )
  end

  @spec append(String.t(), {binary(), list({any(), any()})}) :: :ok
  def append(node_id, {id, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:append, {id, kvlist}},
      :infinity
    )
  end

  @spec add(String.t(), {binary(), %{write: list(), append: list()}}) :: any()
  def add(node_id, {id, map}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:add, {id, map}},
      :infinity
    )
  end

  @spec order(String.t(), [binary()]) :: :ok
  def order(node_id, txs) do
    GenServer.cast(Registry.via(node_id, __MODULE__), {:order, txs})
  end

  ############################################################
  #                      Public Filters                      #
  ############################################################

  def tx_id_filter(tx_id) do
    %__MODULE__.TxIdFilter{tx_id: tx_id}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({write_opt, {tx_id, args}}, from, state)
      when write_opt in [:write, :append, :add] do
    handle_write(write_opt, {tx_id, args}, from, state)

    {:noreply, state}
  end

  def handle_call({:read, {tx_id, key}}, from, state) do
    handle_read({tx_id, key}, from, state)
    {:noreply, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:order, tx_id_list}, state) do
    {:noreply, handle_order(tx_id_list, state)}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec handle_write(
          Storage.write_opts(),
          {binary(), [any()]},
          GenServer.from(),
          t()
        ) :: any()
  defp handle_write(write_opt, {tx_id, args}, from, state) do
    call = &chose_write_function(write_opt).(state.node_id, &1)

    with {:ok, height} <- Map.fetch(state.tx_id_to_height, tx_id) do
      Task.start(fn ->
        GenServer.reply(from, call.({height, args}))
      end)
    else
      _ ->
        node_id = state.node_id

        block_spawn(
          tx_id,
          fn ->
            blocking_write(node_id, {tx_id, args}, from)
          end,
          node_id
        )
    end
  end

  @spec handle_read({binary(), any()}, GenServer.from(), t()) :: any()
  defp handle_read({tx_id, key}, from, state) do
    with {:ok, height} <- Map.fetch(state.tx_id_to_height, tx_id) do
      Task.start(fn ->
        GenServer.reply(from, Storage.read(state.node_id, {height - 1, key}))
      end)

      {:noreply, state}
    else
      _ ->
        node_id = state.node_id

        block_spawn(
          tx_id,
          fn ->
            blocking_read(node_id, {tx_id, key}, from)
          end,
          node_id
        )

        {:noreply, state}
    end
  end

  @spec handle_order(list(binary()), t()) :: t()
  defp handle_order(tx_id_list, state) do
    {map, next_order} =
      for tx_id <- tx_id_list,
          reduce: {state.tx_id_to_height, state.next_height} do
        {map, order} ->
          order_event =
            Node.Event.new_with_body(state.node_id, %__MODULE__.OrderEvent{
              tx_id: tx_id
            })

          EventBroker.event(order_event)
          {Map.put(map, tx_id, order), order + 1}
      end

    %__MODULE__{state | tx_id_to_height: map, next_height: next_order}
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec chose_write_function(Storage.write_opts()) ::
          (String.t(), {non_neg_integer(), list() | map()} ->
             any())
  defp chose_write_function(:write), do: &Storage.write/2
  defp chose_write_function(:append), do: &Storage.append/2
  defp chose_write_function(:add), do: &Storage.add/2

  ############################################################
  #                      Private Filters                     #
  ############################################################

  defp this_module_filter() do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  ############################################################
  #                    Blocking Operations                   #
  ############################################################

  defp block_spawn(id, call, node_id) do
    {:ok, pid} =
      Task.start(call)

    EventBroker.subscribe(pid, [
      Node.Event.node_filter(node_id),
      this_module_filter(),
      tx_id_filter(id)
    ])
  end

  @spec blocking_read(String.t(), {binary(), any()}, GenServer.from()) :: :ok
  defp blocking_read(node_id, {id, key}, from) do
    block(from, id, fn -> read(node_id, {id, key}) end, node_id)
  end

  @spec blocking_write(String.t(), {binary(), [any()]}, GenServer.from()) ::
          :ok
  defp blocking_write(node_id, {id, kvlist}, from) do
    block(
      from,
      id,
      fn ->
        write(node_id, {id, kvlist})
      end,
      node_id
    )
  end

  @spec block(GenServer.from(), binary(), (-> any()), String.t()) :: :ok
  defp block(from, tx_id, call, node_id) do
    receive do
      %EventBroker.Event{
        body: %Node.Event{body: %__MODULE__.OrderEvent{tx_id: ^tx_id}}
      } ->
        result = call.()
        GenServer.reply(from, result)

      _ ->
        IO.puts("this should be unreachable")
    end

    EventBroker.unsubscribe_me([
      Node.Event.node_filter(node_id),
      this_module_filter(),
      tx_id_filter(tx_id)
    ])
  end
end
