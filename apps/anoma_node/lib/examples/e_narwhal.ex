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
  alias Anoma.Node.Transaction.Narwhal.Bullshark
  alias Anoma.Node.Transaction.Narwhal.Cert
  alias Anoma.Node.Transaction.Narwhal.Config
  alias Anoma.Node.Transaction.Narwhal.Events
  alias Anoma.Node.Transaction.Narwhal.Primary
  alias Anoma.Node.Transaction.Narwhal.Supervisor, as: NarwhalSup
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

  @doc """
  I verify all validators produce the same consensus ordering.

  Submits one transaction per validator and feeds the protocol
  until all appear in consensus from every validator.
  """
  @spec all_validators_agree([Config.t()]) :: [Config.t()]
  def all_validators_agree(configs \\ generate_validator_configs()) do
    start_all_validators(configs, batch_size: 1)

    for c <- configs do
      subscribe_local(c.node_id, %Events.NarwhalConsensusFilter{})
    end

    tx_ids =
      for c <- configs, do: Mempool.tx(c.node_id, ETransaction.bluf())

    feeder = spawn_link(fn -> feed_transactions(configs) end)

    min_txs = length(tx_ids)

    orderings =
      collect_consensus_all(
        Enum.map(configs, & &1.node_id),
        &(length(&1) >= min_txs)
      )

    stop_feeder(feeder)

    [first | rest] = orderings
    for order <- rest, do: assert(order == first)
    for tx <- tx_ids, do: assert(tx in first)

    configs
  end

  @doc """
  I verify all validators reach identical execution state.

  Builds on `all_validators_agree/1`: after ordering is proven
  identical, submits counter transactions (zero + increment)
  and compares every validator's values table. This proves
  identical execution, not just identical ordering.
  """
  @spec all_validators_execute_identically([Config.t()]) :: [Config.t()]
  def all_validators_execute_identically(
        configs \\ generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    key = "key"

    for c <- configs do
      subscribe_local(c.node_id, %Mempool.Events.BlockFilter{})
    end

    Mempool.tx(hd(configs).node_id, ETransaction.zero(key))
    inc_id = Mempool.tx(hd(configs).node_id, ETransaction.inc(key))

    feeder = spawn_link(fn -> feed_transactions(configs) end)

    for c <- configs do
      await_block_with_tx(c.node_id, inc_id)
    end

    stop_feeder(feeder)

    backups = for c <- configs, do: dump_shard_backups(c.node_id)
    [first | rest] = backups
    for b <- rest, do: assert(b == first)

    # Verify the counter progressed: zero set 0, inc set 1
    counter_vals =
      first
      |> Enum.filter(fn {{_shard, k, _h}, _v} -> k == ["key"] end)
      |> Enum.sort_by(fn {{_, _, h}, _} -> h end)
      |> Enum.map(fn {_, v} -> v end)

    assert counter_vals == [0, 1]

    configs
  end

  @doc """
  I start 3 of 4 validators and achieve consensus.

  With n=4 and f=1, three validators meet the 2f+1=3 quorum.
  The 4th validator is offline. All three early validators
  agree on the ordering.
  """
  @spec partial_network_commits([Config.t()]) :: [Config.t()]
  def partial_network_commits(configs \\ generate_validator_configs()) do
    [c1, c2, c3, _c4] = configs
    early = [c1, c2, c3]

    start_all_validators(early, batch_size: 1)

    for c <- early do
      subscribe_local(c.node_id, %Events.NarwhalConsensusFilter{})
    end

    Mempool.tx(c1.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(early) end)
    orders = for c <- early, do: elem(receive_consensus(c.node_id), 0)
    stop_feeder(feeder)

    [first | rest] = orders
    for order <- rest, do: assert(order == first)

    configs
  end

  @doc """
  I demonstrate a late-joining validator catching up.

  Builds on `partial_network_commits/1`: after 3 validators
  commit, the 4th joins, fast-forwards its Primary round via
  certificate synchronization, and participates in consensus.
  """
  @spec late_validator_catches_up([Config.t()]) :: [Config.t()]
  def late_validator_catches_up(configs \\ generate_validator_configs()) do
    configs = partial_network_commits(configs)
    [c1, _, _, c4] = configs

    subscribe_local(c4.node_id, %Events.NarwhalConsensusFilter{})
    start_narwhal_node(c4, batch_size: 1)

    # Submit to an early validator (c4 is at round 0 and its blocks
    # would be rejected as stale by validators at higher rounds).
    tx_id = Mempool.tx(c1.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(configs) end)
    order = collect_consensus(c4.node_id, &(tx_id in &1))
    stop_feeder(feeder)

    assert tx_id in order

    {_state_name, primary_data} =
      :sys.get_state(Registry.via(c4.node_id, Primary))

    assert primary_data.shared.round > 0,
           "Primary should have fast-forwarded"

    configs
  end

  @doc """
  I clear all locally-stored blocks from one validator.

  Builds on `late_validator_catches_up/1`: after all 4 validators
  are running, I wipe the first validator's blocks table.
  """
  @spec missing_blocks([Config.t()]) :: [Config.t()]
  def missing_blocks(configs \\ generate_validator_configs()) do
    configs = late_validator_catches_up(configs)
    clear_blocks(hd(configs).node_id)
    configs
  end

  @doc """
  I verify that missing blocks are recovered from peers.

  Builds on `missing_blocks/1`: after clearing, the next commit's
  causal traversal calls ensure_block, fetches blocks from peers
  across multiple rounds, and ordering continues.
  """
  @spec missing_blocks_recovered_via_peer([Config.t()]) :: [Config.t()]
  def missing_blocks_recovered_via_peer(
        configs \\ generate_validator_configs()
      ) do
    configs = missing_blocks(configs)
    target = hd(configs)

    tx_id = Mempool.tx(target.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(configs) end)
    order = collect_consensus(target.node_id, &(tx_id in &1))
    stop_feeder(feeder)

    assert tx_id in order

    # Causal traversal fetched historical blocks from peers.
    # Verify blocks span multiple rounds (the traversal walked
    # backwards through the DAG to rebuild local state).
    table = NarwhalSup.blocks_table(target.node_id)

    {:atomic, recovered} =
      :mnesia.transaction(fn ->
        :mnesia.match_object({table, :_, :_})
      end)

    rounds =
      recovered
      |> Enum.map(fn {_, _, block} -> block.round end)
      |> Enum.uniq()
      |> Enum.sort()

    assert length(rounds) >= 3,
           "Expected blocks from at least 3 rounds, got #{inspect(rounds)}"

    configs
  end

  @doc """
  I verify each tx_id appears exactly once in the ordering.

  Submits three rounds of transactions (12 total across 4
  validators) to span multiple waves. Asserts uniqueness and
  agreement across all validators for the cumulative ordering.
  """
  @spec each_tx_ordered_once([Config.t()]) :: [Config.t()]
  def each_tx_ordered_once(configs \\ generate_validator_configs()) do
    start_all_validators(configs, batch_size: 1)

    for c <- configs do
      subscribe_local(c.node_id, %Events.NarwhalConsensusFilter{})
    end

    tx_ids =
      for _round <- 1..3,
          c <- configs,
          do: Mempool.tx(c.node_id, ETransaction.bluf())

    feeder = spawn_link(fn -> feed_transactions(configs) end)

    target_set = MapSet.new(tx_ids)

    orderings =
      collect_consensus_all(
        Enum.map(configs, & &1.node_id),
        &MapSet.subset?(target_set, MapSet.new(&1))
      )

    stop_feeder(feeder)

    for order <- orderings do
      assert order == Enum.uniq(order), "Ordering contains duplicates"
    end

    [first | rest] = orderings
    for o <- rest, do: assert(o == first)

    configs
  end

  @doc """
  I verify that a skipped wave leader does not block consensus.

  The leader for wave 1 (sorted_validators[1]) stays offline.
  Bullshark skips wave 1 and commits wave 2+. All transactions
  are eventually ordered through causal traversal of later waves.
  """
  @spec skipped_leader_recovery([Config.t()]) :: [Config.t()]
  def skipped_leader_recovery(configs \\ generate_validator_configs()) do
    sorted = Config.sorted_validators(hd(configs))
    wave_1_leader_pk = Enum.at(sorted, rem(1, length(sorted)))

    {[_offline], active} =
      Enum.split_with(configs, &(&1.public_key == wave_1_leader_pk))

    start_all_validators(active, batch_size: 1)

    target = hd(active)
    subscribe_local(target.node_id, %Events.NarwhalConsensusFilter{})

    tx_id = Mempool.tx(target.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(active) end)

    # Collect consensus events until wave 2+ commits
    events = collect_waves_until_wave(target.node_id, 2)
    stop_feeder(feeder)

    all_orders = Enum.flat_map(events, &elem(&1, 0))
    wave_numbers = Enum.map(events, &elem(&1, 1))

    assert tx_id in all_orders, "Transaction should be ordered"
    refute 1 in wave_numbers, "Wave 1 should be skipped (leader offline)"
    assert Enum.max(wave_numbers) >= 2

    configs
  end

  @doc """
  I verify an offline validator's transactions are still ordered.

  Builds on `all_validators_agree/1`: after consensus, one
  validator submits a transaction and then goes offline. The
  remaining three continue advancing rounds. The offline
  validator's transaction — already batched and certified before
  suspension — gets ordered through Bullshark's causal traversal
  of the DAG.
  """
  @spec offline_validator_transactions_ordered([Config.t()]) :: [Config.t()]
  def offline_validator_transactions_ordered(
        configs \\ generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    {active, [victim]} = Enum.split(configs, 3)

    # Submit a transaction to the victim.
    # batch_size: 1 → immediate flush → victim's Primary
    # includes the batch digest in its next block proposal.
    tx_id = Mempool.tx(victim.node_id, ETransaction.bluf())

    # Feed all validators so the victim's block gets certified.
    feeder = spawn_link(fn -> feed_transactions(configs) end)
    {first_order, _} = receive_consensus(hd(active).node_id)

    # Suspend the victim — it can no longer participate.
    for mod <- [Primary, Worker, Bullshark] do
      :sys.suspend(Registry.via(victim.node_id, mod))
    end

    stop_feeder(feeder)

    # Feed only active validators to advance rounds.
    # Bullshark's causal traversal commits the victim's
    # certified blocks from before the suspension.
    feeder = spawn_link(fn -> feed_transactions(active) end)
    order = collect_consensus(hd(active).node_id, &(tx_id in &1), first_order)
    stop_feeder(feeder)

    assert tx_id in order,
           "Offline validator's transaction should be ordered"

    for mod <- [Primary, Worker, Bullshark] do
      :sys.resume(Registry.via(victim.node_id, mod))
    end

    configs
  end

  @doc """
  I verify consensus continues after one validator crashes.

  Builds on `all_validators_agree/1`: after all four validators
  commit, one validator is suspended (simulating a crash). The
  remaining three still meet quorum (2f+1=3) and produce
  consensus normally.
  """
  @spec consensus_after_validator_crash([Config.t()]) :: [Config.t()]
  def consensus_after_validator_crash(configs \\ generate_validator_configs()) do
    configs = all_validators_agree(configs)
    surviving = Enum.take(configs, 3)
    crashed = List.last(configs)

    # Suspend all Narwhal processes for the crashed validator
    for mod <- [Primary, Worker, Bullshark] do
      :sys.suspend(Registry.via(crashed.node_id, mod))
    end

    # Submit tx to a surviving validator
    tx_id = Mempool.tx(hd(surviving).node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(surviving) end)

    orders =
      collect_consensus_all(
        Enum.map(surviving, & &1.node_id),
        &(tx_id in &1)
      )

    stop_feeder(feeder)

    [first | rest] = orders
    for o <- rest, do: assert(o == first)
    assert tx_id in first

    # Resume suspended processes (cleanup)
    for mod <- [Primary, Worker, Bullshark] do
      :sys.resume(Registry.via(crashed.node_id, mod))
    end

    configs
  end

  @doc """
  I verify a crashed validator recovers and rejoins consensus.

  Builds on `consensus_after_validator_crash/1`: after the
  suspended validator is resumed, it catches up via its
  backlogged events. A transaction submitted to the recovered
  validator is ordered by all four validators.
  """
  @spec crashed_validator_recovers([Config.t()]) :: [Config.t()]
  def crashed_validator_recovers(configs \\ generate_validator_configs()) do
    configs = consensus_after_validator_crash(configs)
    recovered = List.last(configs)

    # Submit tx to the recovered validator
    tx_id = Mempool.tx(recovered.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(configs) end)

    # Verify ALL validators (including recovered) order the tx.
    # The recovered Bullshark must have caught up and committed
    # the wave containing this tx.
    collect_consensus_all(
      Enum.map(configs, & &1.node_id),
      &(tx_id in &1)
    )

    stop_feeder(feeder)

    configs
  end

  ############################################################
  #        Byzantine Behavior — Invariant Testing            #
  ############################################################

  # Invariants:
  #
  # 1. DAG has at most one cert per {validator, round}.
  # 2. DAG keeps advancing rounds (liveness).
  # 3. Blocks reference >= 2f+1 certificates from round r-1.

  @doc """
  I generate configs where node 0 and a 5th node share a keypair.

  validator_set has `n` unique public keys; node_id_set has `n+1`
  node_ids. The duplicate reuses the first validator's keypair.
  """
  @spec generate_duplicate_validator_configs(pos_integer()) :: [Config.t()]
  def generate_duplicate_validator_configs(n \\ 4) do
    configs = Config.generate_configs(n)
    original = hd(configs)
    dupe_id = original.node_id <> "_dupe"
    all_ids = MapSet.put(original.node_id_set, dupe_id)

    dupe = %Config{original | node_id: dupe_id, node_id_set: all_ids}
    configs = Enum.map(configs, &%{&1 | node_id_set: all_ids})
    assert dupe.public_key == hd(configs).public_key
    configs ++ [dupe]
  end

  @doc """
  I run 5 nodes where two share a keypair and verify honest
  validators agree and every cert has quorum signatures.

  Invariant 1: `signed_blocks` prevents honest validators from
  signing both proposals for the same {creator, round}. By
  pigeonhole, at most one reaches quorum.

  Invariant 2: the 3 uncontested validators form quorum (3/4)
  and keep advancing despite the contested slot.
  """
  @spec duplicate_validators_cannot_equivocate([Config.t()]) :: [Config.t()]
  def duplicate_validators_cannot_equivocate(
        configs \\ generate_duplicate_validator_configs()
      ) do
    start_all_validators(configs, batch_size: 1)
    honest = Enum.take(configs, 4)
    tx_id = Mempool.tx(hd(honest).node_id, ETransaction.bluf())

    orders = feed_and_collect(configs, honest, &(tx_id in &1))

    for order <- orders, do: assert(tx_id in order)
    [first | rest] = orders
    for o <- rest, do: assert(o == first)

    for c <- honest, do: assert_all_certs_have_quorum(c.node_id, c)

    configs
  end

  @doc """
  I verify every block at round r > 0 references >= 2f+1
  certificates from round r-1.

  Invariant 3: blocks are properly anchored in the DAG.
  """
  @spec blocks_reference_quorum_certs([Config.t()]) :: [Config.t()]
  def blocks_reference_quorum_certs(
        configs \\ generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    observer = hd(configs)

    feed_and_collect(configs, [observer], &(length(&1) >= 3))
    assert_blocks_reference_quorum(observer.node_id, observer)

    configs
  end

  @doc """
  I verify Bullshark only commits waves where the anchor has
  >= f+1 vote-round references.

  Invariant 4: the commit threshold is respected.
  """
  @spec bullshark_respects_commit_threshold([Config.t()]) :: [Config.t()]
  def bullshark_respects_commit_threshold(
        configs \\ generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    observer = hd(configs)

    feed_and_collect(configs, [observer], &(length(&1) >= 5))
    assert_commits_have_threshold(observer.node_id, observer)

    configs
  end

  # Helpers: DAG invariant predicates
  #
  # Pure inspectors — read state, return true or raise.
  # No setup, no side effects.

  @spec get_bullshark_state(String.t()) :: Bullshark.t()
  defp get_bullshark_state(node_id) do
    :sys.get_state(Registry.via(node_id, Bullshark))
  end

  @spec get_bullshark_dag(String.t()) ::
          %{{binary(), non_neg_integer()} => Cert.t()}
  defp get_bullshark_dag(node_id) do
    get_bullshark_state(node_id).dag
  end

  @spec assert_all_certs_have_quorum(String.t(), Config.t()) :: :ok
  defp assert_all_certs_have_quorum(node_id, config) do
    quorum = Config.quorum(config)

    for {_key, cert} <- get_bullshark_dag(node_id) do
      assert Cert.has_quorum?(cert, quorum)
    end

    :ok
  end

  @spec assert_blocks_reference_quorum(String.t(), Config.t()) :: :ok
  defp assert_blocks_reference_quorum(node_id, config) do
    dag = get_bullshark_dag(node_id)
    quorum = Config.quorum(config)

    checked =
      for {{_pk, round}, cert} <- dag,
          round > 0,
          block = NarwhalSup.get_block(node_id, cert.block_digest),
          block != nil do
        assert length(block.certificates) >= quorum
        for c <- block.certificates, do: assert(c.round == round - 1)
        round
      end

    assert length(checked) > 0, "No blocks with round > 0 found"
    :ok
  end

  @spec assert_commits_have_threshold(String.t(), Config.t()) :: :ok
  defp assert_commits_have_threshold(node_id, config) do
    state = get_bullshark_state(node_id)
    dag = state.dag
    threshold = Config.commit_threshold(config)

    for wave <- 0..state.last_committed_wave, wave >= 0 do
      anchor_round = wave * 2
      vote_round = wave * 2 + 1
      leader = Bullshark.wave_leader(config, wave)

      case Map.get(dag, {leader, anchor_round}) do
        nil ->
          :ok

        anchor_cert ->
          refs =
            Enum.count(Config.sorted_validators(config), fn validator ->
              with %Cert{} = vote_cert <- Map.get(dag, {validator, vote_round}),
                   block when block != nil <-
                     NarwhalSup.get_block(node_id, vote_cert.block_digest) do
                Enum.any?(block.certificates, fn c ->
                  c.block_digest == anchor_cert.block_digest and
                    c.validator == anchor_cert.validator
                end)
              else
                _ -> false
              end
            end)

          assert refs >= threshold,
                 "Wave #{wave}: #{refs} vote refs, need #{threshold}"
      end
    end

    :ok
  end

  # Helpers: consensus lifecycle
  #
  # feed_and_collect bundles the subscribe/feed/collect/stop
  # boilerplate that most multi-validator examples repeat.

  @spec feed_and_collect(
          [Config.t()],
          [Config.t()],
          ([binary()] -> boolean())
        ) :: [[binary()]]
  defp feed_and_collect(feed_configs, observe_configs, done?) do
    for c <- observe_configs do
      subscribe_local(c.node_id, %Events.NarwhalConsensusFilter{})
    end

    feeder = spawn_link(fn -> feed_transactions(feed_configs) end)

    orders =
      collect_consensus_all(
        Enum.map(observe_configs, & &1.node_id),
        done?
      )

    stop_feeder(feeder)
    orders
  end

  # Helpers: protocol liveness

  @spec feed_transactions([Config.t()], non_neg_integer()) :: no_return()
  defp feed_transactions(configs, interval \\ 200) do
    Process.sleep(interval)

    for c <- configs do
      try do
        Mempool.tx(c.node_id, ETransaction.bluf())
      catch
        :exit, _ -> :ok
      end
    end

    feed_transactions(configs, interval)
  end

  @spec stop_feeder(pid()) :: true
  defp stop_feeder(pid) do
    Process.unlink(pid)
    Process.exit(pid, :kill)
  end

  # Helpers: block store manipulation

  @spec clear_blocks(String.t()) :: {:atomic, :ok}
  defp clear_blocks(node_id) do
    {:atomic, :ok} = :mnesia.clear_table(NarwhalSup.blocks_table(node_id))
  end

  # Helpers: node lifecycle

  @spec start_all_validators([Config.t()], keyword()) :: [term()]
  defp start_all_validators(configs, opts) do
    for c <- configs, do: start_narwhal_node(c, opts)
  end

  @spec start_narwhal_node(Config.t(), keyword()) :: term()
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

  @spec subscribe_vs(Config.t(), struct()) :: :ok
  defp subscribe_vs(config, filter) do
    EventBroker.subscribe_me([
      %Events.ValidatorSetFilter{node_ids: config.node_id_set},
      filter
    ])
  end

  @spec subscribe_local(String.t(), struct()) :: :ok
  defp subscribe_local(node_id, filter) do
    EventBroker.subscribe_me([
      Anoma.Node.Event.node_filter(node_id),
      filter
    ])
  end

  # Helpers: await events

  @spec await_tx_event(binary(), timeout()) :: term()
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

  @spec await_proposal(String.t(), timeout()) :: Block.t()
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

  @spec await_certificate(timeout()) :: Cert.t()
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

  @spec await_dissemination(String.t(), timeout()) :: {binary(), [binary()]}
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

  @spec await_batch_ready(binary(), timeout()) :: term()
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

  @spec await_consensus(timeout()) :: [binary()]
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

  @spec await_block_event(timeout()) :: {[binary()], non_neg_integer()}
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

  @spec receive_consensus(String.t(), timeout()) ::
          {[binary()], non_neg_integer()}
  defp receive_consensus(node_id, timeout \\ 30_000) do
    receive do
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          node_id: ^node_id,
          body: %Events.NarwhalConsensusEvent{order: order, round: round}
        }
      } ->
        {order, round}
    after
      timeout ->
        raise "Timeout waiting for consensus from #{inspect(node_id)}"
    end
  end

  @spec collect_consensus(
          String.t(),
          ([binary()] -> boolean()),
          [binary()],
          timeout()
        ) :: [binary()]
  defp collect_consensus(node_id, done?, acc \\ [], timeout \\ 30_000) do
    if done?.(acc) do
      acc
    else
      {order, _round} = receive_consensus(node_id, timeout)
      collect_consensus(node_id, done?, acc ++ order, timeout)
    end
  end

  # Collect consensus from multiple validators simultaneously.
  # Receives events for ANY validator as they arrive, avoiding
  # mailbox buildup from sequential per-validator collection.
  @spec collect_consensus_all(
          [String.t()],
          ([binary()] -> boolean()),
          timeout()
        ) :: [[binary()]]
  defp collect_consensus_all(node_ids, done?, timeout \\ 30_000) do
    accs = Map.new(node_ids, fn id -> {id, []} end)
    deadline = System.monotonic_time(:millisecond) + timeout
    results = do_collect_all(node_ids, accs, done?, deadline)
    Enum.map(node_ids, fn id -> Map.fetch!(results, id) end)
  end

  @spec do_collect_all(
          [String.t()],
          %{String.t() => [binary()]},
          ([binary()] -> boolean()),
          integer()
        ) :: %{String.t() => [binary()]}
  defp do_collect_all(node_ids, accs, done?, deadline) do
    if Enum.all?(node_ids, fn id -> done?.(Map.fetch!(accs, id)) end) do
      accs
    else
      remaining = max(deadline - System.monotonic_time(:millisecond), 0)

      receive do
        %EventBroker.Event{
          body: %Anoma.Node.Event{
            node_id: node_id,
            body: %Events.NarwhalConsensusEvent{order: order}
          }
        }
        when is_map_key(accs, node_id) ->
          accs = Map.update!(accs, node_id, &(&1 ++ order))
          do_collect_all(node_ids, accs, done?, deadline)
      after
        remaining ->
          status =
            Enum.map(node_ids, fn id ->
              acc = Map.fetch!(accs, id)
              {id, length(acc), done?.(acc)}
            end)

          raise "Timeout waiting for consensus: #{inspect(status)}"
      end
    end
  end

  @spec collect_waves_until_wave(
          String.t(),
          non_neg_integer(),
          [{[binary()], non_neg_integer()}]
        ) :: [{[binary()], non_neg_integer()}]
  defp collect_waves_until_wave(node_id, target_wave, events \\ []) do
    max_wave =
      events |> Enum.map(&elem(&1, 1)) |> Enum.max(fn -> -1 end)

    if max_wave >= target_wave do
      events
    else
      {order, wave} = receive_consensus(node_id)
      collect_waves_until_wave(node_id, target_wave, [{order, wave} | events])
    end
  end

  # Helpers: verification

  @spec verify_storage(String.t(), [binary()]) :: true
  defp verify_storage(node_id, order) do
    blocks_table = Storage.blocks_table(node_id)

    {:atomic, block} =
      :mnesia.transaction(fn ->
        :mnesia.read({blocks_table, ["anoma", "block", 1]})
      end)

    [{^blocks_table, ["anoma", "block", 1], tx_list}] = block
    assert length(tx_list) == length(order)
  end

  @spec await_block_with_tx(String.t(), binary(), [binary()]) :: [binary()]
  defp await_block_with_tx(node_id, target_tx, acc \\ []) do
    if target_tx in acc do
      acc
    else
      receive do
        %EventBroker.Event{
          body: %Anoma.Node.Event{
            node_id: ^node_id,
            body: %Mempool.Events.BlockEvent{order: order}
          }
        } ->
          await_block_with_tx(node_id, target_tx, acc ++ order)
      after
        30_000 ->
          raise "Timeout: #{inspect(node_id)} never executed " <>
                  "tx #{inspect(target_tx)}"
      end
    end
  end

  @spec dump_shard_backups(String.t()) :: [{term(), term()}]
  defp dump_shard_backups(node_id) do
    table = Anoma.Tables.table_shard_backups(node_id)

    {:atomic, result} =
      :mnesia.transaction(fn ->
        :mnesia.select(table, [{{table, :"$1", :"$2"}, [], [:"$$"]}])
      end)

    result |> Enum.map(fn [k, v] -> {k, v} end) |> Enum.sort()
  end
end
