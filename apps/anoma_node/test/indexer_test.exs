defmodule IndexerTest do
  use ExUnit.Case, async: false

  alias Anoma.Node.Examples.EIndexer

  test "indexing examples" do
    # commented out until multihoming
    # EIndexer.indexer_reads_height()
    EIndexer.indexer_reads_nullifier()
    # EIndexer.indexer_reads_nullifiers()
    # EIndexer.indexer_reads_commitments()
    # EIndexer.indexer_does_not_read_revealed()
    # EIndexer.indexer_reads_unrevealed()
  end
end
