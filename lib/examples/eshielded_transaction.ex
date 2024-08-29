defmodule Examples.EShieldedTransaction do
  alias Anoma.Node.{Storage, Ordering, Router}
  alias Anoma.Node.Executor.Worker
  alias Anoma.ShieldedResource.{
    PartialTransaction,
    ProofRecord,
    ShieldedTransaction
  }

  @spec setup_environment() :: :ok
  def setup_environment do
    storage = %Storage{
      qualified: AnomaTest.Worker.Qualified,
      order: AnomaTest.Worker.Order
    }

    {:ok, router, _} = Router.start()

    {:ok, storage} =
      Router.start_engine(router, Storage, storage)

    {:ok, ordering} =
      Router.start_engine(router, Ordering, storage: storage)

    snapshot_path = [:my_special_nock_snaphsot | 0]

    env = %Nock{snapshot_path: snapshot_path, ordering: ordering}

    id = System.unique_integer([:positive])
    {:ok, topic} = Router.new_topic(router)
    :ok = Router.call(router, {:subscribe_topic, topic, :local})
    Storage.ensure_new(storage)
    Ordering.reset(ordering)

    compliance_inputs = """
    {
    "input": {
        "logic" : "0x78641adee85319d58ec95e4d1d4127d96a9ca365e77b5e06f286e71f9d6ca70",
        "label" : "0x12",
        "quantity" : "0x13",
        "data" : "0x14",
        "eph" : true,
        "nonce" : "0x26",
        "npk" : "0x7752582c54a42fe0fa35c40f07293bb7d8efe90e21d8d2c06a7db52d7d9b7a1",
        "rseed" : "0x48"
    },
    "output": {
        "logic" : "0x78641adee85319d58ec95e4d1d4127d96a9ca365e77b5e06f286e71f9d6ca70",
        "label" : "0x12",
        "quantity" : "0x13",
        "data" : "0x812",
        "eph" : true,
        "nonce" : "0x104",
        "npk" : "0x195",
        "rseed" : "0x16"
    },
    "input_nf_key": "0x1",
    "merkle_path": [{"fst": "0x33", "snd": true}, {"fst": "0x83", "snd": false}, {"fst": "0x73", "snd": false}, {"fst": "0x23", "snd": false}, {"fst": "0x33", "snd": false}, {"fst": "0x43", "snd": false}, {"fst": "0x53", "snd": false}, {"fst": "0x3", "snd": false}, {"fst": "0x36", "snd": false}, {"fst": "0x37", "snd": false}, {"fst": "0x118", "snd": false}, {"fst": "0x129", "snd": false}, {"fst": "0x12", "snd": true}, {"fst": "0x33", "snd": false}, {"fst": "0x43", "snd": false}, {"fst": "0x156", "snd": true}, {"fst": "0x63", "snd": false}, {"fst": "0x128", "snd": false}, {"fst": "0x32", "snd": false}, {"fst": "0x230", "snd": true}, {"fst": "0x3", "snd": false}, {"fst": "0x33", "snd": false}, {"fst": "0x223", "snd": false}, {"fst": "0x2032", "snd": true}, {"fst": "0x32", "snd": false}, {"fst": "0x323", "snd": false}, {"fst": "0x3223", "snd": false}, {"fst": "0x203", "snd": true}, {"fst": "0x31", "snd": false}, {"fst": "0x32", "snd": false}, {"fst": "0x22", "snd": false}, {"fst": "0x23", "snd": true}],
    "rcv": "0x3",
    "eph_root": "0x4"
    }
    """

    Process.put(:test_env, %{
      env: env,
      router: router,
      id: id,
      topic: topic,
      storage: storage,
      compliance_inputs: compliance_inputs
    })

    :ok
  end

  @spec generate_compliance_proof() :: none()
  def generate_compliance_proof do
    %{compliance_inputs: compliance_inputs} = Process.get(:test_env)
    {:ok, proof} = ProofRecord.generate_compliance_proof(compliance_inputs)
    Process.put(:compliance_proof, proof)
    :ok
  end

  @spec create_partial_transaction() :: :ok
  def create_partial_transaction do
    proof = Process.get(:compliance_proof)
    input_resource_logic = proof
    output_resource_logic = proof

    ptx = %PartialTransaction{
      logic_proofs: [input_resource_logic, output_resource_logic],
      compliance_proofs: [proof]
    }
    Process.put(:partial_transaction, ptx)
    :ok
  end

  @spec create_shielded_transaction() :: :ok
  def create_shielded_transaction do
    ptx = Process.get(:partial_transaction)
    priv_keys = :binary.copy(<<0>>, 31) <> <<3>>

    shielded_tx =
      %ShieldedTransaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }
      |> ShieldedTransaction.finalize()

    Process.put(:shielded_transaction, shielded_tx)
    :ok
  end

  @spec create_invalid_shielded_transaction() :: :ok
  def create_invalid_shielded_transaction do
    ptx = Process.get(:partial_transaction)
    # Use an invalid private key (all zeros) to create an invalid transaction
    invalid_priv_keys = :binary.copy(<<0>>, 32)

    invalid_shielded_tx =
      %ShieldedTransaction{
        partial_transactions: [ptx],
        delta: invalid_priv_keys
      }
      |> ShieldedTransaction.finalize()

    Process.put(:invalid_shielded_transaction, invalid_shielded_tx)
    :ok
  end
  @spec prepare_executor_transaction(atom()) :: :ok
  def prepare_executor_transaction(type \\ :valid) do
    shielded_tx = case type do
      :valid -> Process.get(:shielded_transaction)
      :invalid -> Process.get(:invalid_shielded_transaction)
    end
    rm_tx_noun = Noun.Nounable.to_noun(shielded_tx)
    executor_tx = [[1 | rm_tx_noun], 0 | 0]
    Process.put(:executor_transaction, {type, executor_tx})
    :ok
  end

  @spec start_worker() :: :ok
  def start_worker do
    %{env: env, router: router, id: id, topic: topic} = Process.get(:test_env)
    {type, executor_tx} = Process.get(:executor_transaction)

    {:ok, spawn} =
      Router.start_engine(
        router,
        Worker,
        {id, {:cairo, executor_tx}, env, topic, nil}
      )

    Process.put(:worker_spawn, {type, spawn})
    :ok
  end


  @spec execute_transaction() :: {:ok, atom()} | {:error, any()}
  def execute_transaction do
    %{env: env, router: router, id: id, topic: topic} = Process.get(:test_env)
    {type, spawn} = Process.get(:worker_spawn)

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(0, id, spawn)
    ])
    Router.send_raw(spawn, {:write_ready, 0})

    result = wait_for_worker_result(spawn)
    :ok = Router.call(router, {:unsubscribe_topic, topic, :local})

    case {type, result} do
      {:valid, :ok} -> {:ok, :valid_transaction_executed}
      {:invalid, :error} -> {:ok, :invalid_transaction_rejected}
      {:valid, :error} -> {:error, :valid_transaction_failed}
      {:invalid, :ok} -> {:error, :invalid_transaction_accepted}
      _ -> {:error, {:unexpected_result, result}}
    end
  end

  defp wait_for_worker_result(worker_addr) do
    receive do
      {:"$gen_cast", {:router_cast, _, {:worker_done, ^worker_addr, status}}} ->
        status
    after
      5000 ->
        {:error, :timeout}
    end
  end


end
