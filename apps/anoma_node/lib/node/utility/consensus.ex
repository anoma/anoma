defmodule Anoma.Node.Utility.Consensus do
  @moduledoc """
  A trivial consensus provider for the mempool.
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Events
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Registry

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  typedstruct do
    field(:interval, non_neg_integer(), default: 5000)
    field(:node_id, String.t())
  end

  deffilter BlockFilter do
    %EventBroker.Event{body: %Node.Event{body: %Events.BlockEvent{}}} ->
      true

    _ ->
      false
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:interval, :node_id])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init(any()) :: {:ok, Consensus.t()}
  def init(args) do
    Process.set_label(__MODULE__)
    args = Keyword.validate!(args, [:interval, :node_id])
    node_id = args[:node_id]

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      block_filter()
    ])

    table =
      String.to_atom(
        "#{Anoma.Node.Logging.Events}_#{:erlang.phash2(args[:node_id])}"
      )

    :mnesia.transaction(fn ->
      for cons <- :mnesia.read(table, :consensus) do
        wait_for_block(cons)
      end
    end)

    start(node_id)

    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  def start(node_id) do
    GenServer.cast(Registry.via(node_id, __MODULE__), :start)
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
      Mempool.tx_dump(node_id) |> Enum.split(10)

    Mempool.execute(node_id, consensus)

    wait_for_block(consensus)

    Process.send_after(self(), :execute, interval)
  end

  def wait_for_block(consensus) do
    receive do
      %EventBroker.Event{
        body: %Node.Event{
          body: %Events.BlockEvent{
            order: ^consensus
          }
        }
      } ->
        :ok
    end
  end

  def block_filter() do
    %__MODULE__.BlockFilter{}
  end
end
