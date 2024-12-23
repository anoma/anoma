defmodule Anoma.Node.Examples.EIndexer do
  alias Anoma.Node
  alias Node.Examples.{ETransaction, ENode}
  alias Node.Transaction.{Storage, Mempool}
  alias Node.Utility.Indexer
  alias Anoma.TransparentResource.Resource

  def indexer_reads_height(node_id \\ Node.example_random_id()) do
    ETransaction.inc_counter_submit_after_read(node_id)
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
        0,
        [
          %Mempool.Tx{
            code: ^zero,
            backend: ^back,
            vm_result: {:ok, [["key" | 0] | 0]},
            tx_result: {:ok, [["key" | 0]]}
          }
        ]
      ]
    ] = Indexer.get(node_id, {:before, 1})

    [] = Indexer.get(node_id, {:before, 0})
  end

  def indexer_reads_after(node_id \\ Node.example_random_id()) do
    ETransaction.inc_counter_submit_after_zero(node_id)
    Indexer.start_link(node_id: node_id)
    {back, inc} = ETransaction.inc("key")

    [
      [
        1,
        [
          %Mempool.Tx{
            code: ^inc,
            backend: ^back,
            vm_result: {:ok, [["key" | 1] | 0]},
            tx_result: {:ok, [["key" | 1]]}
          }
        ]
      ]
    ] = Indexer.get(node_id, {:after, 0})

    [] = Indexer.get(node_id, {:after, 1})
  end

  def indexer_reads_latest(node_id \\ Node.example_random_id()) do
    ETransaction.inc_counter_submit_after_zero(node_id)
    Indexer.start_link(node_id: node_id)
    {back, inc} = ETransaction.inc("key")

    [
      [
        1,
        [
          %Mempool.Tx{
            code: ^inc,
            backend: ^back,
            vm_result: {:ok, [["key" | 1] | 0]},
            tx_result: {:ok, [["key" | 1]]}
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

    nlf = %Resource{} |> Resource.nullifier()
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

    nlf1 =
      %Resource{nonce: :crypto.hash(:sha256, "random1")}
      |> Resource.nullifier()

    nlf2 =
      %Resource{nonce: :crypto.hash(:sha256, "random2")}
      |> Resource.nullifier()

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

    resource1 =
      %Resource{nonce: :crypto.hash(:sha256, "random1")}
      |> Resource.nullifier()

    resource2 =
      %Resource{nonce: :crypto.hash(:sha256, "random2")}
      |> Resource.nullifier()

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
    nul1 = resource |> Resource.nullifier()
    com1 = resource |> Resource.commitment()
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
    nul1 = resource |> Resource.nullifier()
    com1 = resource |> Resource.commitment()

    com2 =
      %Resource{nonce: :crypto.hash(:sha256, "random2")}
      |> Resource.commitment()

    nulfs = MapSet.new([nul1])
    coms = MapSet.new([com1, com2])

    write_new(updates, values, [1], nulfs, coms)

    newset = MapSet.new([com2])
    ^newset = Indexer.get(node_id, :unrevealed)

    node_id
  end

  def indexer_filters_owner(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    Indexer.start_link(node_id: node_id)
    updates = Storage.updates_table(node_id)
    values = Storage.values_table(node_id)

    res1 = %Resource{rseed: "random1", nullifier_key: "jeremy"}
    res2 = %Resource{rseed: "random2", nullifier_key: "michael"}
    list = [res1, res2]

    write_new(
      updates,
      values,
      [1],
      nil,
      list |> Enum.map(&Resource.commitment/1) |> MapSet.new()
    )

    jeremy = Noun.atom_binary_to_integer("jeremy")
    michael = Noun.atom_binary_to_integer("michael")

    2 = Indexer.get(node_id, {:filter, []}) |> MapSet.size()
    1 = Indexer.get(node_id, {:filter, [{:owner, jeremy}]}) |> MapSet.size()
    1 = Indexer.get(node_id, {:filter, [{:owner, michael}]}) |> MapSet.size()

    node_id
  end

  defp write_new(updates, values, heights, nlfs, coms) do
    :mnesia.transaction(fn ->
      if nlfs do
        :mnesia.write({updates, :nullifiers, heights})
      end

      if coms do
        :mnesia.write({updates, :commitments, heights})
      end

      :mnesia.write({values, {hd(heights), :nullifiers}, nlfs})
      :mnesia.write({values, {hd(heights), :commitments}, coms})
    end)
  end
end
