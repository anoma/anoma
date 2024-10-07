defmodule Anoma.Node.Utility.Consensus do
  @moduledoc """
  A trivial consensus provider for the mempool.
  """

  alias Anoma.Node.Transaction.Mempool
  alias __MODULE__

  use TypedStruct

  typedstruct do
    field(:interval, non_neg_integer(), default: 5000)
  end

  def start_link(interval \\ 5000) do
    GenServer.start_link(__MODULE__, interval, name: Consensus)
  end

  @spec init(any()) :: {:ok, Consensus.t()}
  def init(interval) do
    EventBroker.subscribe_me([
      block_filter()
    ])

    :mnesia.transaction(fn ->
      for cons <- :mnesia.read(Anoma.Node.Logging.Events, :consensus) do
        wait_for_block(cons)
      end
    end)

    start()

    {:ok, %__MODULE__{interval: interval}}
  end

  def start() do
    GenServer.cast(__MODULE__, :start)
  end

  def handle_cast(:start, state) do
    execute(state.interval)
    {:noreply, state}
  end

  def handle_info(:execute, state) do
    execute(state.interval)
    {:noreply, state}
  end

  def execute(interval) do
    {consensus, _} = Mempool.tx_dump() |> Enum.reverse() |> Enum.split(10)

    Mempool.execute(consensus)

    wait_for_block(consensus)

    Process.send_after(self(), :execute, interval)
  end

  def wait_for_block(consensus) do
    receive do
      %EventBroker.Event{
        body: %Mempool.BlockEvent{
          order: ^consensus
        }
      } ->
        :ok
    end
  end

  def block_filter() do
    %Consensus.BlockFilter{}
  end
end
