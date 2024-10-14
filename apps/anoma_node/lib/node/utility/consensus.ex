defmodule Anoma.Node.Utility.Consensus do
  @moduledoc """
  A trivial consensus provider for the mempool.
  """

  alias __MODULE__
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Registry

  use TypedStruct

  typedstruct do
    field(:interval, non_neg_integer(), default: 5000)
    field(:node_id, String.t())
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:interval, :node_id])
    name = Registry.name(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init(any()) :: {:ok, Consensus.t()}
  def init(args) do
    Process.set_label(__MODULE__)
    args = Keyword.validate!(args, [:interval, :node_id])

    EventBroker.subscribe_me([
      block_filter()
    ])

    table =
      String.to_atom(
        "#{Anoma.Node.Logging.Events}_#{:erlang.phash2(args[:node_id])}"
      )

    :mnesia.transaction(fn ->
      for cons <- :mnesia.read(table, :consensus) do
        IO.puts("wait for block")
        wait_for_block(cons)
      end
    end)

    start(args[:node_id])

    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  def start(node_id) do
    pid = Registry.whereis(node_id, __MODULE__)
    GenServer.cast(pid, :start)
  end

  def handle_cast(:start, state) do
    execute(state.node_id, state.interval)
    {:noreply, state}
  end

  def handle_info(:execute, state) do
    execute(state.node_id, state.interval)
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def execute(node_id, interval) do
    {consensus, _} =
      Mempool.tx_dump(node_id) |> Enum.reverse() |> Enum.split(10)

    Mempool.execute(node_id, consensus)

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
