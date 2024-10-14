defmodule Anoma.Node.Utility.Indexer do
  @moduledoc """
  A trivial indexer querying the node tables.
  """

  alias __MODULE__
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Storage
  alias Anoma.Crypto.Id

  use GenServer
  use TypedStruct

  typedstruct do
    field(:node_id, Id.t())
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  def init(args) do
    Process.set_label(__MODULE__)

    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  @spec get(String.t(), :nlfs | :cms | :resources | :blocks) :: any()
  def get(node_id, flag) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.call(name, flag)
  end

  def handle_call(:blocks, _from, state) do
    table = Storage.blocks_table(state.node_id)

    {:atomic, res} =
      :mnesia.transaction(fn ->
        case :mnesia.all_keys(table) |> Enum.sort(:desc) do
          [] -> :absent
          [hd | tl] -> hd
        end
      end)

    {:reply, res, state}
  end

  def handle_call(:nlfs, _from, state) do
    {:reply, read_set(:nullifiers, state.node_id), state}
  end

  def handle_call(:cms, _from, state) do
    {:reply, unnulified_coms(state.node_id), state}
  end

  def handle_call(:resources, _from, state) do
    res = unnulified_coms(state.node_id) |> Enum.map(&Nock.Cue.cue!/1)
    {:reply, res, state}
  end

  defp unnulified_coms(id) do
    nullifiers = read_set(:nullifiers, id)
    commitments = read_set(:commitments, id)

    MapSet.difference(nullifiers, commitments)
  end

  defp read_set(key, id) do
    values = Storage.values_table(id)
    updates = Storage.updates_table(id)

    {:atomic, set} =
      :mnesia.transaction(fn ->
        case :mnesia.read(updates, key) do
          [] ->
            MapSet.new([])

          [{^updates, key, list}] ->
            :mnesia.read(values, {hd(list), key}) |> hd() |> elem(2)
        end
      end)

    set
  end
end
