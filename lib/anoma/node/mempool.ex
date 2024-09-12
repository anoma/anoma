defmodule Anoma.Node.Mempool do
  @moduledoc """

  """

  alias Anoma.{Block, Transaction, Serializer}
  alias Anoma.Block.Base
  alias __MODULE__
  alias Anoma.Node.Mempool.Ordering
  alias Anoma.Node.Router

  use TypedStruct

  @typedoc """
  I am a list of transaction candidates.
  """
  @type transactions :: list(Transaction.t())

  @type tx_result :: pid() | {:ok, Noun.t()} | :error

  typedstruct do
    field(:transactions, %{binary() => Noun.t()}, default: %{})
    field(:round, non_neg_integer(), default: 0)

    field(:key, {Serializer.public_key(), Serializer.private_key()},
      default: :crypto.generate_key(:rsa, {1024, 65537})
    )

    field(:logger, Router.Addr.t(), enforce: false)
  end

  def start_link(default) do
    GenServer.start_link(__MODULE__, default, name: Mempool)
  end

  @spec init(any()) :: {:ok, Mempool.t()}
  def init(_arg) do
    __MODULE__.Ordering.start_link(nil)
    __MODULE__.Storage.start_link(nil)
    {:ok, %__MODULE__{}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  def tx_dump(server) do
    GenServer.call(server, :dump)
  end

  def tx(server, tx_w_backend, reply_to \\ nil, id \\ nil) do
    GenServer.cast(server, {:tx, tx_w_backend, reply_to, id})
  end

  # list of ids seen as ordered transactions
  @spec execute(GenServer.server(), list(binary())) :: :ok
  def execute(server, ordered_list_of_txs) do
    GenServer.cast(server, {:execute, ordered_list_of_txs})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:dump, _from, state) do
    {:reply, state.transactions |> Map.keys()}
  end

  def handle_cast({:tx, tx_code_w_backend, reply_to, id}, state) do
    tx_id =
      case id do
        nil -> :crypto.strong_rand_bytes(16)
        id -> id
      end

    Task.start(fn ->
      Anoma.Node.Mempool.Backends.execute(tx_code_w_backend, tx_id, reply_to)
    end)

    nstate = %Mempool{
      state
      | transactions: Map.put(state.transactions, tx_id, tx_code_w_backend)
    }

    {:noreply, nstate}
  end

  def handle_cast({:execute, list}, state) do
    with true <-
           list |> Enum.all(fn x -> Map.has_key?(state.transactions, x) end) do
      res_list = Ordering.order(list)

      {writes, rem} =
        for {res, id, height} <- res_list,
            reduce: {[], state.transactions} do
          {block_writes, _remaining_txs} ->
            res =
              case res do
                {:ok, res} ->
                  {{height, res}, Map.get(state.transactions, id)}

                :error ->
                  {:error, Map.get(state.transactions, id)}
              end

            {[res | block_writes], Map.delete(state.transactions, id)}
        end

      Mempool.Storage.commit(state.round, writes)
      IO.puts("==============BLOCK FINISHED<>COMMIT FINISHED=============")

      {:noreply,
       %__MODULE__{state | transactions: rem, round: state.round + 1}}
    else
      false -> {:noreply, state}
    end
  end

  def handle_call(_, _, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
