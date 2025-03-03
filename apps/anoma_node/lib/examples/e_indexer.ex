defmodule Anoma.Node.Examples.EIndexer do
  alias Anoma.Node
  alias Node.Examples.{ETransaction, ENode}
  alias Node.Transaction.{Storage, Mempool}
  alias Node.Utility.Indexer
  alias Anoma.RM.Transparent.{Resource, Transaction}

  def indexer_reads_height(node_id \\ Node.example_random_id()) do
    ETransaction.zero_counter_submit(node_id)
    Indexer.start_link(node_id: node_id)
    1 = Indexer.get(node_id, :height)

    node_id
  end

  def indexer_reads_before(node_id \\ Node.example_random_id()) do
    ETransaction.inc_counter_submit_after_zero(node_id)
    Indexer.start_link(node_id: node_id)
    {back, zero} = ETransaction.zero("key")

    [
      [
        1,
        [
          %Mempool.Tx{
            code: ^zero,
            backend: ^back,
            vm_result: {:ok, [[["key"] | 0] | 0]},
            tx_result: {:ok, [[["key"] | 0]]}
          }
        ]
      ]
    ] = Indexer.get(node_id, {:before, 2})

    [] = Indexer.get(node_id, {:before, 1})
  end

  def indexer_reads_after(node_id \\ Node.example_random_id()) do
    ETransaction.inc_counter_submit_after_zero(node_id)
    Indexer.start_link(node_id: node_id)
    {back, inc} = ETransaction.inc("key")

    [
      [
        2,
        [
          %Mempool.Tx{
            code: ^inc,
            backend: ^back,
            vm_result: {:ok, [[["key"] | 1] | 0]},
            tx_result: {:ok, [[["key"] | 1]]}
          }
        ]
      ]
    ] = Indexer.get(node_id, {:after, 1})

    [] = Indexer.get(node_id, {:after, 2})
  end

  def indexer_reads_latest(node_id \\ Node.example_random_id()) do
    ETransaction.inc_counter_submit_after_zero(node_id)
    Indexer.start_link(node_id: node_id)
    {back, inc} = ETransaction.inc("key")

    [
      [
        2,
        [
          %Mempool.Tx{
            code: ^inc,
            backend: ^back,
            vm_result: {:ok, [[["key"] | 1] | 0]},
            tx_result: {:ok, [[["key"] | 1]]}
          }
        ]
      ]
    ] = Indexer.get(node_id, :latest_block)
  end

  def indexer_reads_nullifier(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    nlf = Resource.nullifier_hash(<<0::256>>, %Resource{})
    set = MapSet.new([nlf])

    write_new(updates, values, [1], set, nil)

    ^set = Indexer.get(node_id, :nlfs)

    node_id
  end

  def indexer_reads_nullifiers(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    nlf1 = Resource.nullifier_hash(<<0::256>>, %Resource{nonce: :crypto.hash(:sha256, "random1")}) |> Noun.atom_integer_to_binary()

    nlf2 = Resource.nullifier_hash(<<0::256>>, %Resource{nonce: :crypto.hash(:sha256, "random2")}) |> Noun.atom_integer_to_binary()

    set = MapSet.new([nlf1, nlf2])

    write_new(updates, values, [1], set, nil)

    ^set = Indexer.get(node_id, :nlfs)

    node_id
  end

  def indexer_reads_commitments(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    resource1 = Resource.nullifier_hash(<<0::256>>, %Resource{nonce: :crypto.hash(:sha256, "random1")}) |> Noun.atom_integer_to_binary()

    resource2 = Resource.nullifier_hash(<<0::256>>, %Resource{nonce: :crypto.hash(:sha256, "random2")}) |> Noun.atom_integer_to_binary

    set = MapSet.new([resource1, resource2])

    write_new(updates, values, [1], nil, set)

    ^set = Indexer.get(node_id, :cms)

    node_id
  end

  def indexer_does_not_read_revealed(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    resource = %Resource{nonce: :crypto.hash(:sha256, "random1")}
    nul1 = Resource.nullifier_hash(<<0::256>>, resource) |> Noun.atom_integer_to_binary()
    com1 = resource |> Resource.commitment_hash() |> Noun.atom_integer_to_binary()
    nulfs = MapSet.new([nul1])
    coms = MapSet.new([com1])

    write_new(updates, values, [1], nulfs, coms)

    newset = MapSet.new([])
    ^newset = Indexer.get(node_id, :unrevealed)

    node_id
  end

  def indexer_reads_unrevealed(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    resource = %Resource{nonce: :crypto.hash(:sha256, "random1")}
    nul1 = Resource.nullifier_hash(<<0::256>>, resource) |> Noun.atom_integer_to_binary()
    com1 = resource |> Resource.commitment_hash() |> Noun.atom_integer_to_binary()

    com2 =
      %Resource{nonce: :crypto.hash(:sha256, "random2")}
      |> Resource.commitment_hash() |> Noun.atom_integer_to_binary

    nulfs = MapSet.new([nul1])
    coms = MapSet.new([com1, com2])

    write_new(updates, values, [1], nulfs, coms)

    newset = MapSet.new([com2])
    ^newset = Indexer.get(node_id, :unrevealed)

    node_id
  end

  def indexer_reads_anchor(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    hash = "I am a root at height 1"

    :mnesia.transaction(fn ->
      :mnesia.write({updates, ["anoma", :anchor |> Atom.to_string()], [1]})

      :mnesia.write(
        {values, {1, ["anoma", :anchor |> Atom.to_string()]}, hash}
      )
    end)

    ^hash = Indexer.get(node_id, :root)

    node_id
  end

  def indexer_reads_recent_anchor(node_id \\ Node.example_random_id()) do
    indexer_reads_anchor(node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    hash = "I am a root at height 2"

    :mnesia.transaction(fn ->
      :mnesia.write({updates, ["anoma", :anchor |> Atom.to_string()], [2, 1]})

      :mnesia.write(
        {values, {2, ["anoma", :anchor |> Atom.to_string()]}, hash}
      )
    end)

    ^hash = Indexer.get(node_id, :root)

    node_id
  end

  @doc """
  I am a dummy resource with rseed random1 and nullifier_key jeremy
  """
  @spec jeremy_resource() :: Resource.t()
  def jeremy_resource do
    %Resource{
      nonce: :crypto.hash(:sha256, "random1"),
      nullifierkeycommitment: Noun.pad_trailing("jeremy", 32)
    }
  end

  @doc """
  I am a dummy resource with rseed random2 and nullifier_key michael
  """
  @spec michael_resource() :: Resource.t()
  def michael_resource do
    %Resource{
      nonce: :crypto.hash(:sha256, "random2"),
      nullifierkeycommitment: Noun.pad_trailing("michael", 32)
    }
  end

  def indexer_filters_owner(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    res1 = jeremy_resource()
    res2 = michael_resource()
    list = [res1, res2]

    write_new(
      updates,
      values,
      [1],
      nil,
      list |> Enum.map(&Resource.commitment_hash/1) |> MapSet.new()
    )

    2 = Indexer.get(node_id, {:filter, []}) |> MapSet.size()

    1 =
      Indexer.get(node_id, {:filter, [{:owner, res1.nullifierkeycommitment}]})
      |> MapSet.size()

    1 =
      Indexer.get(node_id, {:filter, [{:owner, res2.nullifierkeycommitment}]})
      |> MapSet.size()

    node_id
  end

  def indexer_works_with_transactions(node_id \\ Node.example_random_id()) do
    Anoma.Node.Examples.ETransaction.submit_successful_trivial_swap(node_id)

    base_swap = Examples.ETransparent.ETransaction.swap_from_actions()

    nulfs = base_swap |> Transaction.nullifiers()
    coms = base_swap |> Transaction.commitments()

    ^nulfs = Indexer.get(node_id, :nlfs)
    ^coms = Indexer.get(node_id, :cms)

    node_id
  end

  defp write_new(updates, values, heights, nlfs, coms) do
    :mnesia.transaction(fn ->
      if nlfs do
        :mnesia.write(
          {updates, ["anoma", :nullifiers |> Atom.to_string()], heights}
        )
      end

      if coms do
        :mnesia.write(
          {updates, ["anoma", :commitments |> Atom.to_string()], heights}
        )
      end

      :mnesia.write(
        {values, {hd(heights), ["anoma", :nullifiers |> Atom.to_string()]},
         nlfs}
      )

      :mnesia.write(
        {values, {hd(heights), ["anoma", :commitments |> Atom.to_string()]},
         coms}
      )
    end)
  end
end
