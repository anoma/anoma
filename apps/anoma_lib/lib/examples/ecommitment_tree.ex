defmodule Examples.ECommitmentTree do
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

  @spec new_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def new_ct(spec \\ sha256_32_spec()) do
    tree = CommitmentTree.new(spec)

    assert tree.size == 0

    tree
  end

  @doc """
  A commitment tree with commits from ETransaction.swap_from_actions/1
  """
  @spec ct_with_trivial_swap(term()) ::
          {CommitmentTree.t(), binary()}
  def ct_with_trivial_swap(spec \\ sha256_32_spec()) do
    tree = new_ct(spec)
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

  @spec babylon_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def babylon_ct(spec \\ sha256_32_spec()) do
    # This resets the table, this binding is important!
    empty_ct = new_ct(spec)

    # It's fine the adding hashes come from sha256. Cuz Cairo poseidon hash also
    # returns 256bits.
    hashes =
      Enum.map(["Londo", "G'kar", "Kosh", "Sinclair", "Ivanova"], fn x ->
        :crypto.hash(:sha256, x)
      end)

    {ct, anchor} = CommitmentTree.add(empty_ct, hashes)

    assert length(hashes) == ct.size

    for {hash, index} <- Enum.with_index(hashes) do
      prove = CommitmentTree.prove(ct, index)
      wrong = CommitmentTree.prove(ct, index + 1)

      assert CommitmentTree.Proof.verify(spec, prove, anchor, hash)
      refute CommitmentTree.Proof.verify(spec, wrong, anchor, hash)
    end

    for hash <- hashes do
      prove = CommitmentTree.prove(ct, hash)
      assert CommitmentTree.Proof.verify(spec, prove, anchor, hash)
    end

    ct
  end

  @spec babylon_ct_new_hash(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def babylon_ct_new_hash(spec \\ sha256_32_spec()) do
    ct = babylon_ct(spec)
    new_hash = :crypto.hash(:sha256, "nice")

    {new_ct, anchor} = CommitmentTree.add(ct, [new_hash])

    path = CommitmentTree.prove(new_ct, new_hash)
    assert CommitmentTree.Proof.verify(spec, path, anchor, new_hash)

    new_ct
  end

  @spec lots_of_inserts_ct(CommitmentTree.Spec.t()) :: CommitmentTree.t()
  def lots_of_inserts_ct(spec \\ sha256_32_spec()) do
    ct = new_ct(spec)

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

    cm_tree = new_ct(cairo_spec)
    input_resource_cm = ECairo.EResource.a_resource_commitment()

    # Insert the input resource to the tree
    {ct, anchor} = CommitmentTree.add(cm_tree, [input_resource_cm])
    # Get the merkle proof of the input resource
    merkle_proof = CommitmentTree.prove(ct, 0)
    merkle_proof_2 = CommitmentTree.prove(ct, input_resource_cm)

    assert merkle_proof == merkle_proof_2

    {ct, merkle_proof, anchor}
  end

  @doc """
  A commitment tree with commits from Examples.ERM.EShielded.ETransaction.a_shielded_transaction/0
  """
  @spec ct_with_trivial_cairo_tx(term()) ::
          {CommitmentTree.t(), binary()}
  def ct_with_trivial_cairo_tx(
        cms,
        spec \\ cairo_poseidon_spec()
      ) do
    tree = new_ct(spec)

    {tree, anchor} = CommitmentTree.add(tree, cms)

    {tree, anchor}
  end
end
