defmodule Anoma.Node.Utility.Indexer do
  @moduledoc """
  A trivial indexer querying the node tables.
  """

  alias Anoma.RM.Transparent.Resource
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Storage

  use GenServer
  use TypedStruct

  typedstruct do
    field(:node_id, String.t())
    field(:filters, %{atom() => (any(), any() -> MapSet.t())})
  end

  @type index_type() ::
          :nlfs
          | :cms
          | :unrevealed
          | :resources
          | :height
          | :root
          | :latest_block
          | {:before | :after, non_neg_integer()}
          | {:filter, [{:owner, any()} | {:kind, binary()}]}

  def start_link(args) do
    owner_filter = fn res, owner -> res.nullifierkeycommitment == owner end
    kind_filter = fn res, kind -> Resource.kind(res) == kind end

    args =
      Keyword.validate!(args, [
        :node_id,
        filters: %{:owner => owner_filter, :kind => kind_filter}
      ])

    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  def init(args) do
    Process.set_label(__MODULE__)

    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  @spec get(String.t(), index_type) :: any()
  def get(node_id, :latest_block) do
    if get_height(node_id) == :absent do
      nil
    else
      get(node_id, {:after, get_height(node_id) - 1})
    end
  end

  def get(node_id, flag) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.call(name, flag)
  end

  def handle_call(:height, _from, state) do
    {:reply, get_height(state.node_id), state}
  end

  def handle_call(:nlfs, _from, state) do
    {:reply, read_set(:nullifiers, state.node_id), state}
  end

  def handle_call(:cms, _from, state) do
    {:reply, read_set(:commitments, state.node_id), state}
  end

  def handle_call({:filter, list}, _from, state) do
    ress =
      unnulified_coms(state.node_id)
      |> res_from_coms
      |> Enum.map(fn x -> x |> Resource.from_noun() |> elem(1) end)

    filtered =
      for {atom, arg} <- list, reduce: ress do
        unfiltered ->
          filter = Map.get(state.filters, atom)
          unfiltered |> Enum.filter(fn x -> filter.(x, arg) end)
      end
      |> Enum.map(&Noun.Nounable.to_noun/1)
      |> MapSet.new()

    {:reply, filtered, state}
  end

  def handle_call(:unrevealed, _from, state) do
    {:reply, unnulified_coms(state.node_id), state}
  end

  def handle_call(:root, _from, state) do
    case read_set(:anchor, state.node_id) do
      hash when is_binary(hash) ->
        {:reply, hash, state}

      _ ->
        {:reply, nil, state}
    end
  end

  def handle_call(:resources, _from, state) do
    res =
      unnulified_coms(state.node_id)
      |> res_from_coms()

    {:reply, res, state}
  end

  def handle_call({flag, height}, _from, state)
      when flag in [:before, :after] do
    table = Storage.blocks_table(state.node_id)

    op =
      case flag do
        :before -> :<
        :after -> :>
      end

    {:atomic, res} =
      :mnesia.transaction(fn ->
        :mnesia.select(table, [
          {{table, :"$1", :"$2"}, [{op, :"$1", height}], [:"$$"]}
        ])
      end)

    {:reply, res, state}
  end

  @spec res_from_coms(MapSet.t()) :: MapSet.t()
  defp res_from_coms(coms) do
    coms
    |> get_jam_info(:commitments)
    |> Stream.map(fn x ->
      {:ok, res} = Noun.Jam.cue(x)
      res
    end)
    |> MapSet.new()
  end

  @spec unnulified_coms(String.t()) :: MapSet.t(binary())
  defp unnulified_coms(id) do
    nullifiers = read_set(:nullifiers, id) |> get_jam_info(:nullifiers)
    commitments = read_set(:commitments, id) |> get_jam_info(:commitments)

    Stream.reject(commitments, &Enum.member?(nullifiers, &1))
    |> Stream.map(fn x -> "CM_" <> x end)
    |> MapSet.new()
  end

  @spec get_jam_info(MapSet.t(binary()), :commitments | :nullifiers) ::
          Enumerable.t()
  defp get_jam_info(set, :commitments) do
            set
            |> Stream.map(&Noun.atom_integer_to_binary/1)
            |> Stream.map(fn <<"CM_", rest::binary>> ->
      rest
    end)
  end

  defp get_jam_info(set, :nullifiers) do
    set
    |> Stream.map(&Noun.atom_integer_to_binary/1)
    |> Stream.map(fn <<"NF_", rest::binary>> ->
      rest
    end)
  end

  @spec read_set(:commitments | :nullifiers | :anchor, String.t()) ::
          MapSet.t(binary()) | binary()
  defp read_set(key, id) do
    values = Storage.values_table(id)
    updates = Storage.updates_table(id)
    key = ["anoma", key |> Atom.to_string()]

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

  @spec get_height(String.t()) :: non_neg_integer() | :absent
  defp get_height(node_id) do
    table = Storage.blocks_table(node_id)

    {:atomic, res} =
      :mnesia.transaction(fn ->
        case :mnesia.all_keys(table) |> Enum.sort(:desc) do
          [] -> :absent
          [hd | _tl] -> hd
        end
      end)

    res
  end
end
