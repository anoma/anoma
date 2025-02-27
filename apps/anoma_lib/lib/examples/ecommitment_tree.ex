defmodule Examples.ECommitmentTree do
  alias Anoma.Node.Tables
  alias Anoma.RM.Transparent.Transaction
  alias Examples.ECairo
  alias Examples.ETransparent.ETransaction

  require ExUnit.Assertions

  import ExUnit.Assertions

  @spec sha256_32_spec() :: CommitmentTree.Spec.t()
  def sha256_32_spec() do
    tree = CommitmentTree.Spec.cm_tree_spec()

    assert tree.key_size == 256
    assert tree.depth == 32
    assert tree.splay == 2

    tree
  end

  @spec cairo_poseidon_spec() :: CommitmentTree.Spec.t()
  def cairo_poseidon_spec() do
    tree = CommitmentTree.Spec.cairo_poseidon_cm_tree_spec()

    assert tree.key_size == 256
    assert tree.depth == 32
    assert tree.splay == 2

    tree
  end

  @spec memory_backed_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def memory_backed_ct(spec \\ sha256_32_spec()) do
    tree = CommitmentTree.new(spec, nil)

    assert tree.size == 0
    assert tree.table == nil

    tree
  end

  @doc """
  A commitment tree with commits from ETransaction.swap_from_actions/1
  """
  @spec memory_backed_ct_with_trivial_swap(term()) ::
          {CommitmentTree.t(), binary()}
  def memory_backed_ct_with_trivial_swap(spec \\ sha256_32_spec()) do
    tree = memory_backed_ct(spec)
    transaction = ETransaction.swap_from_actions()

    commits = Transaction.commitments(transaction)

    {tree, anchor} =
      CommitmentTree.add(
        tree,
        commits |> Enum.map(&Noun.atom_integer_to_binary/1)
      )

    assert tree.size == MapSet.size(commits)

    {tree, anchor}
  end

  @spec empty_mnesia_backed_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def empty_mnesia_backed_ct(spec \\ sha256_32_spec()) do
    CommitmentTree.init_storage("no_node")

    table_name = Tables.table_commitment_tree()
    Tables.clear_table(Tables.table_commitment_tree())
    tree = CommitmentTree.new(spec, table_name)

    assert tree.size == 0
    assert tree.table == table_name

    tree
  end

  # @doc """
  # This fetches the current mnesia tree storage

  # This value is expected to differ, and will be a fixture for other
  # tests to assert about.
  # """
  @spec current_tree_mnesia_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def current_tree_mnesia_ct(spec) do
    table_name = Tables.table_commitment_tree()
    tree = CommitmentTree.new(spec, table_name)

    assert :mnesia.table_info(table_name, :size) == tree.size

    tree
  end

  @spec babylon_mnesia_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def babylon_mnesia_ct(spec \\ sha256_32_spec()) do
    # This resets the table, this binding is important!
    empty_ct = empty_mnesia_backed_ct(spec)

    # It's fine the adding hashes come from sha256. Cuz Cairo poseidon hash also
    # returns 256bits.
    hashes =
      Enum.map(["Londo", "G'kar", "Kosh", "Sinclair", "Ivanova"], fn x ->
        :crypto.hash(:sha256, x)
      end)

    {ct, anchor} = CommitmentTree.add(empty_ct, hashes)

    assert length(hashes) == ct.size

    restored_tc = current_tree_mnesia_ct(spec)

    assert ct == restored_tc, "Restoring from storage gives the same tree"

    for {hash, index} <- Enum.with_index(hashes) do
      prove = CommitmentTree.prove(ct, index)
      wrong = CommitmentTree.prove(ct, index + 1)

      assert CommitmentTree.Proof.verify(spec, prove, anchor, hash)
      refute CommitmentTree.Proof.verify(spec, wrong, anchor, hash)
    end

    ct
  end

  @spec lots_of_inserts_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def lots_of_inserts_ct(spec \\ sha256_32_spec()) do
    ct = memory_backed_ct(spec)

    {ct_batches, keys} =
      Enum.reduce(1..100, {ct, []}, fn _, {ct, keys} ->
        new_keys =
          Enum.map(0..:rand.uniform(25), fn _ ->
            :crypto.strong_rand_bytes(32)
          end)

        {ct, _anchor} = CommitmentTree.add(ct, new_keys)
        {ct, keys ++ new_keys}
      end)

    {ct_allatonce, _anchor} = CommitmentTree.add(ct, keys)

    assert ct_batches == ct_allatonce,
           "adding 2,500 keys in batches and all at once is the same"

    ct_oneatatime =
      Enum.reduce(keys, ct, fn cm, ct ->
        elem(CommitmentTree.add(ct, [cm]), 0)
      end)

    assert ct_batches == ct_oneatatime,
           "adding 2,500 keys in batches and one at the time is the same"

    ct_batches
  end

  @spec a_merkle_proof() ::
          {CommitmentTree.t(), CommitmentTree.Proof.t(), any()}
  def a_merkle_proof() do
    cairo_spec = cairo_poseidon_spec()

    cm_tree = empty_mnesia_backed_ct(cairo_spec)
    input_resource_cm = ECairo.EResource.a_resource_commitment()

    # Insert the input resource to the tree
    {ct, anchor} = CommitmentTree.add(cm_tree, [input_resource_cm])
    # Get the merkle proof of the input resource
    merkle_proof = CommitmentTree.prove(ct, 0)

    {ct, merkle_proof, anchor}
  end

  @doc """
  A commitment tree with commits from Examples.ERM.EShielded.ETransaction.a_shielded_transaction/0
  """
  @spec memory_backed_ct_with_trivial_cairo_tx(term()) ::
          {CommitmentTree.t(), binary()}
  def memory_backed_ct_with_trivial_cairo_tx(
        cms,
        spec \\ cairo_poseidon_spec()
      ) do
    tree = memory_backed_ct(spec)

    {tree, anchor} = CommitmentTree.add(tree, cms)

    {tree, anchor}
  end
end
