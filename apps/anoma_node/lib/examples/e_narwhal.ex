defmodule Anoma.Node.Examples.ENarwhal do
  @moduledoc """
  I contain examples for the Narwhal + Bullshark consensus protocol.

  Examples compose progressively: higher-level ones call lower-level
  ones for setup, following the shard examples pattern.

  Config -> Block -> Cert (data types, no nodes)
  Worker accumulate -> flush -> disseminate (single/dual node)
  Primary round 0 cert -> Bullshark wave 0 -> Full execution
  """

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Narwhal.Block
  alias Anoma.Node.Transaction.Narwhal.Cert
  alias Anoma.Node.Transaction.Narwhal.Config
  alias Anoma.Node.Transaction.Narwhal.Events
  alias Anoma.Node.Transaction.Narwhal.Worker
  alias Anoma.Node.Transaction.Storage

  require Anoma.Node.Event

  import ExUnit.Assertions

  @spec generate_validator_configs() :: [Config.t()]
  def generate_validator_configs() do
    configs = Config.generate_configs(4)
    config = hd(configs)

    4 = Config.n(config)
    1 = Config.f(config)
    3 = Config.quorum(config)
    2 = Config.commit_threshold(config)

    [a, b | _] = configs
    assert Config.sorted_validators(a) == Config.sorted_validators(b)

    configs
  end

  @spec create_block(Config.t()) :: Block.t()
  def create_block(config \\ hd(generate_validator_configs())) do
    digest = :crypto.hash(:sha256, "example batch")

    %Block{
      batch_digests: [digest],
      certificates: [],
      round: 0,
      creator: config.public_key
    }
  end

  @spec sign_a_block(Config.t()) :: Block.t()
  def sign_a_block(config \\ hd(generate_validator_configs())) do
    block = create_block(config)

    false = Block.valid?(block)
    signed = Block.sign(block, config.secret_key)
    true = Block.valid?(signed)

    # Digest is stable across signing
    block_digest = Block.digest(block)
    ^block_digest = Block.digest(signed)

    signed
  end

  @spec build_certificate([Config.t()]) :: Cert.t()
  def build_certificate(configs \\ generate_validator_configs()) do
    signed_block = sign_a_block(hd(configs))
    block_digest = Block.digest(signed_block)
    config = hd(configs)

    cert = %Cert{
      block_digest: block_digest,
      validator: config.public_key,
      round: 0
    }

    false = Cert.has_quorum?(cert, Config.quorum(config))

    cert =
      configs
      |> Enum.take(3)
      |> Enum.reduce(cert, fn c, acc ->
        sig = Anoma.Crypto.Sign.sign_detached(block_digest, c.secret_key)
        Cert.add_signature(acc, c.public_key, sig)
      end)

    3 = map_size(cert.signatures)
    true = Cert.has_quorum?(cert, Config.quorum(config))

    true =
      Cert.verified_quorum?(
        cert,
        Config.quorum(config),
        config.validator_set
      )

    cert
  end

  @spec worker_accumulates_transaction([Config.t()]) :: [Config.t()]
  def worker_accumulates_transaction(configs \\ generate_validator_configs()) do
    config = hd(configs)
    start_narwhal_node(config)

    subscribe_local(config.node_id, %Mempool.Events.TxFilter{})
    tx_id = Mempool.tx(config.node_id, ETransaction.bluf())
    await_tx_event(tx_id)

    state = :sys.get_state(Registry.via(config.node_id, Worker))
    assert tx_id in Enum.map(state.batch, &elem(&1, 0))

    configs
  end

  @spec worker_flushes_full_batch([Config.t()]) :: [Config.t()]
  def worker_flushes_full_batch(configs \\ generate_validator_configs()) do
    config = hd(configs)
    start_narwhal_node(config, batch_size: 2)

    subscribe_vs(config, %Events.BatchDisseminateFilter{})
    subscribe_local(config.node_id, %Events.BatchReadyFilter{})

    Mempool.tx(config.node_id, ETransaction.bluf())
    Mempool.tx(config.node_id, ETransaction.bluf())

    {digest, tx_ids} = await_dissemination(config.node_id)
    2 = length(tx_ids)
    await_batch_ready(digest)

    {:ok, ^tx_ids} = Worker.get_batch(config.node_id, digest)
    [] = :sys.get_state(Registry.via(config.node_id, Worker)).batch

    configs
  end

  @spec worker_disseminates_to_peer([Config.t()]) :: [Config.t()]
  def worker_disseminates_to_peer(configs \\ generate_validator_configs()) do
    [config_a, config_b | _] = configs

    start_narwhal_node(config_a, batch_size: 1)
    start_narwhal_node(config_b, batch_size: 10)
    subscribe_vs(config_a, %Events.BatchDisseminateFilter{})

    Mempool.tx(config_a.node_id, ETransaction.bluf())
    {digest, _} = await_dissemination(config_a.node_id)

    {:ok, _} = Worker.get_batch(config_a.node_id, digest)

    # Sync: ensure node_b's Worker has processed the event
    :sys.get_state(Registry.via(config_b.node_id, Worker))
    {:ok, _} = Worker.get_batch(config_b.node_id, digest)

    configs
  end

  @spec primary_round_0_certificate([Config.t()]) :: [Config.t()]
  def primary_round_0_certificate(configs \\ generate_validator_configs()) do
    start_all_validators(configs, batch_size: 1)
    config = hd(configs)

    subscribe_vs(config, %Events.BlockProposalFilter{})
    subscribe_vs(config, %Events.CertificateFilter{})

    Mempool.tx(config.node_id, ETransaction.bluf())

    block = await_proposal(config.node_id)
    0 = block.round
    true = Block.valid?(block)

    cert = await_certificate()
    0 = cert.round
    true = Cert.has_quorum?(cert, Config.quorum(config))

    configs
  end

  @spec all_validators_share_transactions([Config.t()]) :: [Config.t()]
  def all_validators_share_transactions(
        configs \\ generate_validator_configs()
      ) do
    start_all_validators(configs, batch_size: 1)
    subscribe_vs(hd(configs), %Events.BatchDisseminateFilter{})

    tx_id = Mempool.tx(hd(configs).node_id, ETransaction.bluf())
    await_dissemination(hd(configs).node_id)

    # Sync Worker queues so register_foreign_txs completes
    for c <- configs,
        do: :sys.get_state(Registry.via(c.node_id, Worker))

    for c <- configs do
      assert tx_id in Mempool.tx_dump(c.node_id),
             "node #{c.node_id} missing foreign tx"
    end

    configs
  end

  @doc """
  I run 2 rounds to complete wave 0, triggering Bullshark.
  Builds on `primary_round_0_certificate/1` which starts all nodes
  and completes round 0.
  """
  @spec bullshark_commits_wave_0([Config.t()]) :: [Config.t()]
  def bullshark_commits_wave_0(configs \\ generate_validator_configs()) do
    primary_round_0_certificate(configs)
    config = hd(configs)

    subscribe_local(config.node_id, %Events.NarwhalConsensusFilter{})

    # Round 1: vote round for Bullshark wave 0
    for c <- configs, do: Mempool.tx(c.node_id, ETransaction.bluf())

    order = await_consensus()
    assert length(order) > 0

    configs
  end

  @doc """
  I verify the full pipeline: consensus -> Mempool -> Executor -> Storage.
  Builds on `bullshark_commits_wave_0/1`.
  """
  @spec narwhal_executes_transactions([Config.t()]) :: [Config.t()]
  def narwhal_executes_transactions(configs \\ generate_validator_configs()) do
    config = hd(configs)
    subscribe_local(config.node_id, %Mempool.Events.BlockFilter{})

    bullshark_commits_wave_0(configs)

    {order, _round} = await_block_event()
    assert length(order) > 0
    verify_storage(config.node_id, order)

    configs
  end

  @doc """
  I run a single validator (n=1) through the full pipeline.
  Minimal proof that Narwhal replaces manual execution ordering.
  """
  @spec single_validator_executes() :: Config.t()
  def single_validator_executes() do
    [config] = Config.generate_configs(1)

    1 = Config.n(config)
    0 = Config.f(config)
    1 = Config.quorum(config)

    subscribe_local(config.node_id, %Mempool.Events.BlockFilter{})
    start_narwhal_node(config, batch_size: 1)

    # Round 0 + Round 1 to trigger wave 0 commit
    Mempool.tx(config.node_id, ETransaction.bluf())
    Mempool.tx(config.node_id, ETransaction.bluf())

    {order, _round} = await_block_event()
    assert length(order) > 0
    verify_storage(config.node_id, order)

    config
  end

  # Helpers: node lifecycle

  defp start_all_validators(configs, opts) do
    for c <- configs, do: start_narwhal_node(c, opts)
  end

  defp start_narwhal_node(config, opts \\ []) do
    ENode.start_node(
      node_id: config.node_id,
      transaction: [
        narwhal: [
          config: config,
          batch_size: Keyword.get(opts, :batch_size, 10)
        ]
      ]
    )
  end

  # Helpers: subscriptions

  defp subscribe_vs(config, filter) do
    EventBroker.subscribe_me([
      %Events.ValidatorSetFilter{node_ids: config.node_id_set},
      filter
    ])
  end

  defp subscribe_local(node_id, filter) do
    EventBroker.subscribe_me([
      Anoma.Node.Event.node_filter(node_id),
      filter
    ])
  end

  # Helpers: await events

  defp await_tx_event(tx_id, timeout \\ 2000) do
    assert_receive(
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          body: %Mempool.Events.TxEvent{id: ^tx_id}
        }
      },
      timeout
    )
  end

  defp await_proposal(node_id, timeout \\ 3000) do
    assert_receive(
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          body: %Events.BlockProposalEvent{
            block: block,
            from_node: ^node_id
          }
        }
      },
      timeout
    )

    block
  end

  defp await_certificate(timeout \\ 5000) do
    assert_receive(
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          body: %Events.CertificateEvent{cert: cert}
        }
      },
      timeout
    )

    cert
  end

  defp await_dissemination(node_id, timeout \\ 5000) do
    assert_receive(
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          body: %Events.BatchDisseminateEvent{
            digest: digest,
            tx_ids: tx_ids,
            from_node: ^node_id
          }
        }
      },
      timeout
    )

    {digest, tx_ids}
  end

  defp await_batch_ready(digest, timeout \\ 2000) do
    assert_receive(
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          body: %Events.BatchReadyEvent{digest: ^digest}
        }
      },
      timeout
    )
  end

  defp await_consensus(timeout \\ 15_000) do
    assert_receive(
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          body: %Events.NarwhalConsensusEvent{order: order}
        }
      },
      timeout
    )

    order
  end

  defp await_block_event(timeout \\ 15_000) do
    assert_receive(
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          body: %Mempool.Events.BlockEvent{
            order: order,
            round: round
          }
        }
      },
      timeout
    )

    {order, round}
  end

  # Helpers: verification

  defp verify_storage(node_id, order) do
    blocks_table = Storage.blocks_table(node_id)

    {:atomic, block} =
      :mnesia.transaction(fn ->
        :mnesia.read({blocks_table, ["anoma", "block", 1]})
      end)

    [{^blocks_table, ["anoma", "block", 1], tx_list}] = block
    assert length(tx_list) == length(order)
  end
end
