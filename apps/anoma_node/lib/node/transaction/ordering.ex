defmodule Anoma.Node.Transaction.Ordering do
  @moduledoc """
  I am the Ordering Engine.

  I act as a mediator between Workers and Storage. In particular, Workers
  working on a transaction may ask to read and write information. However,
  they do not know when to do it, they only know the ID of the transaction
  they work on.

  I process such requests, keeping them waiting until consensus provides
  some ordering to a transaction in question. Once they do, I pair a
  transaction ID with its timestamp and forward queries to Storage.

  ### Public API

  I provide the following public functonality:

  - `read/2`
  - `write/2`
  - `append/2`
  - `add/2`
  - `order/2`
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Storage
  alias Anoma.Node.Events

  require Node.Event

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc """
  Type of the arguments the ordering genserver expects
  """
  @type args_t ::
          [
            node_id: String.t(),
            next_height: non_neg_integer()
          ]
          | [node_id: String.t()]
  ############################################################
  #                         State                            #
  ############################################################

  typedstruct enforce: true do
    @typedoc """
    I am the type of the Ordering Enigine.

    I contain the Node for which the Ordering is launched, the upcoming
    hight as well as a map from transaction IDs to their global order.

    ### Fields

    - `:node_id` - The ID of the Node to which an Ordering instantiation is
                   bound.
    - `:next_height` - The height that the next ordered transaction
                       candidate will get.
                       Default: 1
    - `:tx_id_to_height` - A map from an ID of a transaction candidate to
                           its order.
    """
    field(:node_id, String.t())
    field(:next_height, integer(), default: 1)
    # maps tx ids to their height for writing.
    # the previous height is used for reading.
    field(:tx_id_to_height, %{binary() => integer()}, default: %{})
  end

  deffilter TxIdFilter, tx_id: binary() do
    %EventBroker.Event{body: %Node.Event{body: %{tx_id: ^tx_id}}} -> true
    _ -> false
  end

  @doc """
  I am the start_link function for the Ordering Engine.

  I register the engine with supplied node ID provided by the arguments.
  """

  @spec start_link(args_t()) :: GenServer.on_start()
  def start_link(args \\ []) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the initialization function for the Ordering Engine.

  From the specified arguments, I get the Node ID as well as the info
  regarding the next height Ordering should be started with.
  """

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, [:node_id, next_height: 1])

    state = struct(Ordering, Enum.into(args, %{}))

    {:ok, state}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the Ordering read function.

  I receive a Node ID and an {id, key} tuple. There are two states possible
  when Ordering processes my request. Either:

  - The id has been assigned an order.
  - The id has not been assigned an order.

  If the former is true, I send the request to read the key at the
  specified height minus one to the Storage. That is, we ask the storage to
  read the most recent value assigned to the key from the point of view of
  the transaction candidate. See `Storage.read/2`.

  If the latter is true, I leave the caller blocked until the id has been
  assigned a value, i.e. until a corresponding event gets received.
  """

  @spec read(String.t(), {binary(), any()}) :: any()
  def read(node_id, {id, key}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:read, {id, key}},
      :infinity
    )
  end

  @doc """
  I am the Ordering write function.

  I receive a Node ID and an {id, kvlist} tuple. There are two states
  possible when Ordering processes my request. Either:

  - The id has been assigned an order.
  - The id has not been assigned an order.

  If the former is true, I send the request to write the key-value list at
  the specified height to the Storage. See `Storage.write/2`

  If the latter is true, I leave the caller blocked until the id has been
  assigned a value, i.e. until a corresponding event gets received.
  """

  @spec write(String.t(), {binary(), list({any(), any()})}) :: :ok
  def write(node_id, {id, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:write, {id, kvlist}},
      :infinity
    )
  end

  @doc """
  I am the Ordering append function.

  I receive a Node ID and an {id, kvlist} tuple. There are two states
  possible when Ordering processes my request. Either:

  - The id has been assigned an order.
  - The id has not been assigned an order.

  If the former is true, I send the request to append the key-value list at
  the specified height to the Storage. See `Storage.append/2`

  If the latter is true, I leave the caller blocked until the id has been
  assigned a value, i.e. until a corresponding event gets received.
  """

  @spec append(String.t(), {binary(), list({any(), MapSet.t()})}) :: :ok
  def append(node_id, {id, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:append, {id, kvlist}},
      :infinity
    )
  end

  @doc """
  I am the Ordering write function.

  I receive a Node ID and an {id, map} tuple. There are two states possible
  when Ordering processes my request. Either:

  - The id has been assigned an order.
  - The id has not been assigned an order.

  If the former is true, I send the request to appropriately add the map to
  the Storage at the specified height. See `Storage.add/2`

  If the latter is true, I leave the caller blocked until the id has been
  assigned a value, i.e. until a corresponding event gets received.
  """

  @spec add(String.t(), {binary(), %{write: list(), append: list()}}) :: any()
  def add(node_id, {id, map}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:add, {id, map}},
      :infinity
    )
  end

  @doc """
  I am the Ordering order function.

  Given a Node ID and a list of transaction IDs, I percieve the latter as a
  partial ordering of transactions. Afterwards, I assign them a global
  ordering by adding the next height stored in the Ordering Engine to the
  respective ordering inside a list.

  Afterwards, I send an event specifying that a particular ID has indeed
  received an order.
  """

  @spec order(String.t(), [binary()]) :: :ok
  def order(node_id, txs) do
    GenServer.cast(Registry.via(node_id, __MODULE__), {:order, txs})
  end

  ############################################################
  #                      Public Filters                      #
  ############################################################

  @doc """
  I am a filter spec which filters for any event with a `tx_id` field and
  matches iff the ID stored is the one supplied.
  """

  @spec tx_id_filter(binary()) :: TxIdFilter.t()
  def tx_id_filter(tx_id) do
    %__MODULE__.TxIdFilter{tx_id: tx_id}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
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

  @impl true
  def handle_cast({:order, tx_id_list}, state) do
    {:noreply, handle_order(tx_id_list, state)}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  @impl true
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
          Events.order_event(tx_id, state.node_id, __MODULE__)
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
        body: %Node.Event{body: %Events.OrderEvent{tx_id: ^tx_id}}
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
