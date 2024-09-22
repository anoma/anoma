defmodule Anoma.Node.Transaction.Mempool do
  @moduledoc """

  """

  alias __MODULE__
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Ordering
  alias Anoma.Node.Transaction.Storage

  use TypedStruct

  typedstruct do
    field(
      :transactions,
      %{binary() => {Backends.backend(), Noun.t()}},
      default: %{}
    )

    field(:round, non_neg_integer(), default: 0)
  end

  def start_link(default) do
    GenServer.start_link(__MODULE__, default, name: Mempool)
  end

  @spec init(any()) :: {:ok, Mempool.t()}
  def init(_arg) do
    {:ok, %__MODULE__{}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  def tx_dump() do
    GenServer.call(__MODULE__, :dump)
  end

  def tx(tx_w_backend) do
    tx(tx_w_backend, :crypto.strong_rand_bytes(16))
  end

  # only to be called by Logging replays directly
  def tx(tx_w_backend, id) do
    GenServer.cast(__MODULE__, {:tx, tx_w_backend, id})
  end

  # list of ids seen as ordered transactions
  @spec execute(list(binary())) :: :ok
  def execute(ordered_list_of_txs) do
    GenServer.call(__MODULE__, {:execute, ordered_list_of_txs})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:dump, _from, state) do
    {:reply, state.transactions |> Map.keys(), state}
  end

  def handle_call({:execute, id_list}, _from, state) do
    with true <-
           id_list
           |> Enum.all?(fn x -> Map.has_key?(state.transactions, x) end) do
      task =
        Task.async(fn ->
          blocking_complete(id_list, state.transactions)
        end)

      for id <- id_list do
        EventBroker.subscribe(task.pid, [
          worker_module_filter(),
          Ordering.tx_id_filter(id)
        ])
      end

      Ordering.order(id_list)

      {writes, rem} = Task.await(task)

      Storage.commit(state.round, writes)

      {:reply, :ok,
       %__MODULE__{state | transactions: rem, round: state.round + 1}}
    else
      false -> {:reply, :error, state}
    end
  end

  def handle_call(_, _, state) do
    {:reply, :ok, state}
  end

  # the pid for ro backend now needs to be specified inside the backend
  def handle_cast({:tx, tx_code_w_backend, tx_id}, state) do
    Task.start(fn ->
      Backends.execute(tx_code_w_backend, tx_id)
    end)

    nstate = %Mempool{
      state
      | transactions: Map.put(state.transactions, tx_id, tx_code_w_backend)
    }

    {:noreply, nstate}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def blocking_complete(ids, map) do
    for _id <- ids,
        reduce: {[], map} do
      {list, rem} ->
        receive do
          %EventBroker.Event{
            body: %Anoma.Node.Transaction.Backends.CompleteEvent{
              tx_id: id,
              result: {:ok, res}
            }
          } ->
            EventBroker.unsubscribe_me([
              worker_module_filter(),
              Ordering.tx_id_filter(id)
            ])

            {[{{:ok, res}, Map.get(rem, id)} | list], Map.delete(rem, id)}

          %EventBroker.Event{
            body: %Anoma.Node.Transaction.Backends.CompleteEvent{
              tx_id: id,
              result: :error
            }
          } ->
            EventBroker.unsubscribe_me([
              worker_module_filter(),
              Ordering.tx_id_filter(id)
            ])

            {[{:error, Map.get(rem, id)} | list], Map.delete(rem, id)}
        end
    end
  end

  def worker_module_filter() do
    %EventBroker.Filters.SourceModule{module: Anoma.Node.Transaction.Backends}
  end
end
