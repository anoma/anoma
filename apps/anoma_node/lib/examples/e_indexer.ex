defmodule Anoma.Node.Examples.EIndexer do
  # alias Anoma.Node.Examples.{ETransaction}
  # alias Anoma.Node.Transaction.Storage
  # alias Anoma.Node.Utility.Indexer
  # alias Anoma.TransparentResource.Resource

  # def indexer_reads_height(node_id \\ "londo_mollari") do
  #   ETransaction.inc_counter_submit_after_read(node_id)
  #   Indexer.start_link(node_id: node_id)
  #   1 = Indexer.get(node_id, :blocks)

  #   node_id
  # end

  # def indexer_reads_nullifier(node_id \\ "londo_mollari") do
  #   ETransaction.restart_storage(node_id)
  #   Indexer.start_link(node_id: node_id)
  #   updates = Storage.updates_table(node_id)
  #   values = Storage.values_table(node_id)

  #   nlf = %Resource{} |> Resource.nullifier()
  #   set = MapSet.new([nlf])

  #   write_new(updates, values, [1], set, nil)

  #   ^set = Indexer.get(node_id, :nlfs)

  #   node_id
  # end

  # def indexer_reads_nullifiers(node_id \\ "londo_mollari") do
  #   ETransaction.restart_storage(node_id)
  #   Indexer.start_link(node_id: node_id)
  #   updates = Storage.updates_table(node_id)
  #   values = Storage.values_table(node_id)

  #   nlf1 = %Resource{rseed: "random1"} |> Resource.nullifier()
  #   nlf2 = %Resource{rseed: "random2"} |> Resource.nullifier()
  #   set = MapSet.new([nlf1, nlf2])

  #   write_new(updates, values, [1], set, nil)

  #   ^set = Indexer.get(node_id, :nlfs)

  #   node_id
  # end

  # def indexer_reads_commitments(node_id \\ "londo_mollari") do
  #   ETransaction.restart_storage(node_id)
  #   Indexer.start_link(node_id: node_id)
  #   updates = Storage.updates_table(node_id)
  #   values = Storage.values_table(node_id)

  #   resource1 = %Resource{rseed: "random1"} |> Resource.nullifier()
  #   resource2 = %Resource{rseed: "random2"} |> Resource.nullifier()
  #   set = MapSet.new([resource1, resource2])

  #   write_new(updates, values, [1], nil, set)

  #   ^set = Indexer.get(node_id, :cms)

  #   node_id
  # end

  # def indexer_does_not_read_revealed(node_id \\ "londo_mollari") do
  #   ETransaction.restart_storage(node_id)
  #   Indexer.start_link(node_id: node_id)
  #   updates = Storage.updates_table(node_id)
  #   values = Storage.values_table(node_id)

  #   resource = %Resource{rseed: "random1"}
  #   nul1 = resource |> Resource.nullifier()
  #   com1 = resource |> Resource.commitment()
  #   nulfs = MapSet.new([nul1])
  #   coms = MapSet.new([com1])

  #   write_new(updates, values, [1], nulfs, coms)

  #   newset = MapSet.new([])
  #   ^newset = Indexer.get(node_id, :unrevealed)

  #   node_id
  # end

  # def indexer_reads_unrevealed(node_id \\ "londo_mollari") do
  #   ETransaction.restart_storage(node_id)
  #   Indexer.start_link(node_id: node_id)
  #   updates = Storage.updates_table(node_id)
  #   values = Storage.values_table(node_id)

  #   resource = %Resource{rseed: "random1"}
  #   nul1 = resource |> Resource.nullifier()
  #   com1 = resource |> Resource.commitment()
  #   com2 = %Resource{rseed: "random2"} |> Resource.commitment()
  #   nulfs = MapSet.new([nul1])
  #   coms = MapSet.new([com1, com2])

  #   write_new(updates, values, [1], nulfs, coms)

  #   newset = MapSet.new([com2])
  #   ^newset = Indexer.get(node_id, :unrevealed)

  #   node_id
  # end

  # defp write_new(updates, values, heights, nlfs, coms) do
  #   :mnesia.transaction(fn ->
  #     if nlfs do
  #       :mnesia.write({updates, :nullifiers, heights})
  #     end

  #     if coms do
  #       :mnesia.write({updates, :commitments, heights})
  #     end

  #     :mnesia.write({values, {hd(heights), :nullifiers}, nlfs})
  #     :mnesia.write({values, {hd(heights), :commitments}, coms})
  #   end)
  # end
end
