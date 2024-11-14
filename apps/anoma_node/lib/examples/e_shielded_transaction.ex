defmodule Anoma.Node.Examples.EShieldedTransaction do
  alias Anoma.Node
  alias Node.Transaction.{Storage, Mempool, Backends}
  alias Examples.{ENock, ETransparent.ETransaction}
  alias Examples.ECairo.EResource, as: ESResource
  alias Anoma.Node.Examples.ETransaction
  require ExUnit.Assertions
  import ExUnit.Assertions

  @spec submit_successful_trivial_cairo_tx(String.t()) :: String.t()
  def submit_successful_trivial_cairo_tx(node_id \\ Node.example_random_id()) do
    ETransaction.start_tx_module(node_id)

    s_tx = Examples.ECairo.ETransaction.a_shielded_transaction()

    tx_w_backend = trivial_cairo_transaction(s_tx)

    EventBroker.subscribe_me([])

    Mempool.tx(node_id, tx_w_backend, "id 1")
    Mempool.execute(node_id, Mempool.tx_dump(node_id))

    ETransaction.recieve_round_event(node_id, 0)

    # Generate the nf and cm from fixed resources
    input_nullifier = ESResource.a_resource_nullifier()

    assert {:ok, MapSet.new([input_nullifier])} ==
             Storage.read(node_id, {1, ["anoma", "cairo_nullifiers"]})

    {tree, anchor} =
      Examples.ECommitmentTree.memory_backed_ct_with_trivial_cairo_tx()

    {_ct, _merkle_proof, old_root} = Examples.ECommitmentTree.a_merkle_proof()

    assert {:ok, MapSet.new([old_root, anchor])} ==
             Storage.read(node_id, {1, ["anoma", "cairo_roots"]})

    assert {:ok, tree} == Storage.read(node_id, {1, ["anoma", "cairo_ct"]})

    commitment = s_tx.commitments |> hd()

    assert {:ok, MapSet.new([{commitment, 0}])} ==
             Storage.read(node_id, {1, ["anoma", "cairo_indices"]})

    proof = CommitmentTree.prove(tree, 0)
    spec = CommitmentTree.Spec.cairo_poseidon_cm_tree_spec()

    assert CommitmentTree.Proof.verify(spec, proof, anchor, commitment) ==
             true

    EventBroker.unsubscribe_me([])

    node_id
  end

  @spec trivial_cairo_transaction() :: {Backends.backend(), Noun.t()}
  def trivial_cairo_transaction(
        s_tx \\ Examples.ECairo.ETransaction.a_shielded_transaction()
      ) do
    noun = s_tx |> Noun.Nounable.to_noun()

    assert Anoma.CairoResource.Transaction.from_noun(noun) == {:ok, s_tx}

    {:cairo_resource, ENock.transparent_core(noun)}
  end
end
