defmodule Anoma.Node.Examples.EShieldedTransaction do
  alias Anoma.Node
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Storage
  alias Examples.ECairo.EResource, as: ESResource
  alias Examples.ENock

  require ExUnit.Assertions

  import ExUnit.Assertions

  @spec submit_successful_trivial_cairo_tx(String.t()) :: String.t()
  def submit_successful_trivial_cairo_tx(node_id \\ Node.example_random_id()) do
    ETransaction.start_tx_module(node_id)

    tx_w_backend = trivial_cairo_transaction()

    EventBroker.subscribe_me([])

    Mempool.tx(node_id, tx_w_backend, "id 1")
    Mempool.execute(node_id, Mempool.tx_dump(node_id))

    ETransaction.recieve_round_event(node_id, 0)

    # Generate the nf and cm from fixed resources
    input_nullifier = ESResource.a_resource_nullifier()

    assert {:ok, MapSet.new([input_nullifier])} ==
             Storage.read(node_id, {1, ["anoma", "cairo_nullifiers"]})

    output_resource_cm =
      ESResource.a_fixed_output_resource()
      |> Anoma.CairoResource.Resource.commitment()

    {tree, anchor} =
      Examples.ECommitmentTree.memory_backed_ct_with_trivial_cairo_tx([
        output_resource_cm
      ])

    set_of_ciphertexts =
      Examples.ECairo.ETransaction.a_shielded_transaction()
      |> Anoma.CairoResource.Transaction.get_cipher_texts()
      |> MapSet.new()

    assert {:ok,
            MapSet.new([Anoma.Constants.default_cairo_rm_root(), anchor])} ==
             Storage.read(node_id, {1, ["anoma", "cairo_roots"]})

    assert {:ok, tree} == Storage.read(node_id, {1, ["anoma", "cairo_ct"]})

    assert {:ok, set_of_ciphertexts} ==
             Storage.read(node_id, {1, ["anoma", "cairo_ciphertexts"]})

    EventBroker.unsubscribe_me([])

    node_id
  end

  @spec submit_successful_complex_cairo_tx(String.t()) :: String.t()
  def submit_successful_complex_cairo_tx(node_id \\ Node.example_random_id()) do
    ETransaction.start_tx_module(node_id)

    tx_w_backend = complex_cairo_transaction()

    set_of_ciphertexts =
      Examples.ECairo.ETransaction.a_shielded_transaction_with_multiple_actions()
      |> Anoma.CairoResource.Transaction.get_cipher_texts()
      |> MapSet.new()

    EventBroker.subscribe_me([])

    Mempool.tx(node_id, tx_w_backend, "id 1")
    Mempool.execute(node_id, Mempool.tx_dump(node_id))

    ETransaction.recieve_round_event(node_id, 0)

    # Generate the nf and cm from fixed resources
    input_nullifier_1 = ESResource.a_resource_nullifier()

    input_nullifier_2 =
      ESResource.a_trivial_input_intent_resource()
      |> Anoma.CairoResource.Resource.nullifier(<<1::256>>)

    assert {:ok, MapSet.new([input_nullifier_1, input_nullifier_2])} ==
             Storage.read(node_id, {1, ["anoma", "cairo_nullifiers"]})

    assert {:ok, set_of_ciphertexts} ==
             Storage.read(node_id, {1, ["anoma", "cairo_ciphertexts"]})

    output_cm_1 =
      ESResource.a_fixed_output_resource()
      |> Anoma.CairoResource.Resource.commitment()

    output_cm_2 =
      ESResource.a_trivial_output_intent_resource()
      |> Anoma.CairoResource.Resource.commitment()

    {tree, anchor} =
      Examples.ECommitmentTree.memory_backed_ct_with_trivial_cairo_tx([
        output_cm_1,
        output_cm_2
      ])

    assert {:ok,
            MapSet.new([Anoma.Constants.default_cairo_rm_root(), anchor])} ==
             Storage.read(node_id, {1, ["anoma", "cairo_roots"]})

    assert {:ok, tree} == Storage.read(node_id, {1, ["anoma", "cairo_ct"]})
    EventBroker.unsubscribe_me([])

    node_id
  end

  @spec submit_successful_multiple_cairo_txs(String.t()) :: String.t()
  def submit_successful_multiple_cairo_txs(
        node_id \\ Node.example_random_id()
      ) do
    ETransaction.start_tx_module(node_id)

    tx_w_backend_1 = trivial_cairo_transaction()

    set_of_ciphertexts1 =
      Examples.ECairo.ETransaction.a_shielded_transaction()
      |> Anoma.CairoResource.Transaction.get_cipher_texts()
      |> MapSet.new()

    EventBroker.subscribe_me([])

    Mempool.tx(node_id, tx_w_backend_1, "id 1")

    tx_w_backend_2 = trivial_cairo_intent_transaction()

    set_of_ciphertexts2 =
      Examples.ECairo.ETransaction.a_shielded_transaction_with_intents()
      |> Anoma.CairoResource.Transaction.get_cipher_texts()
      |> MapSet.new()

    Mempool.tx(node_id, tx_w_backend_2, "id 2")
    Mempool.execute(node_id, Mempool.tx_dump(node_id))

    ETransaction.recieve_round_event(node_id, 0)

    # Generate the nf and cm from fixed resources
    input_nullifier_1 = ESResource.a_resource_nullifier()

    input_nullifier_2 =
      ESResource.a_trivial_input_intent_resource()
      |> Anoma.CairoResource.Resource.nullifier(<<1::256>>)

    assert {:ok, MapSet.new([input_nullifier_1, input_nullifier_2])} ==
             Storage.read(node_id, {2, ["anoma", "cairo_nullifiers"]})

    output_cm_1 =
      ESResource.a_fixed_output_resource()
      |> Anoma.CairoResource.Resource.commitment()

    output_cm_2 =
      ESResource.a_trivial_output_intent_resource()
      |> Anoma.CairoResource.Resource.commitment()

    {_, anchor_1} =
      Examples.ECommitmentTree.memory_backed_ct_with_trivial_cairo_tx([
        output_cm_1
      ])

    {tree, anchor_2} =
      Examples.ECommitmentTree.memory_backed_ct_with_trivial_cairo_tx([
        output_cm_1,
        output_cm_2
      ])

    assert {:ok,
            MapSet.new([
              Anoma.Constants.default_cairo_rm_root(),
              anchor_1,
              anchor_2
            ])} ==
             Storage.read(node_id, {2, ["anoma", "cairo_roots"]})

    assert {:ok, tree} == Storage.read(node_id, {2, ["anoma", "cairo_ct"]})

    set_of_ciphertexts =
      MapSet.union(set_of_ciphertexts1, set_of_ciphertexts2)

    assert {:ok, set_of_ciphertexts} ==
             Storage.read(node_id, {2, ["anoma", "cairo_ciphertexts"]})

    EventBroker.unsubscribe_me([])

    node_id
  end

  @spec trivial_cairo_transaction() :: {Backends.backend(), Noun.t()}
  def trivial_cairo_transaction() do
    s_tx = Examples.ECairo.ETransaction.a_shielded_transaction()
    noun = s_tx |> Noun.Nounable.to_noun()

    assert Anoma.CairoResource.Transaction.from_noun(noun) == {:ok, s_tx}

    {:cairo_resource, ENock.transparent_core(noun)}
  end

  @spec trivial_cairo_intent_transaction() :: {Backends.backend(), Noun.t()}
  def trivial_cairo_intent_transaction() do
    s_tx =
      Examples.ECairo.ETransaction.a_shielded_transaction_with_intents()

    noun = s_tx |> Noun.Nounable.to_noun()

    assert Anoma.CairoResource.Transaction.from_noun(noun) == {:ok, s_tx}

    {:cairo_resource, ENock.transparent_core(noun)}
  end

  @spec complex_cairo_transaction() :: {Backends.backend(), Noun.t()}
  def complex_cairo_transaction() do
    s_tx =
      Examples.ECairo.ETransaction.a_shielded_transaction_with_multiple_actions()

    noun = s_tx |> Noun.Nounable.to_noun()

    assert Anoma.CairoResource.Transaction.from_noun(noun) == {:ok, s_tx}

    {:cairo_resource, ENock.transparent_core(noun)}
  end
end
