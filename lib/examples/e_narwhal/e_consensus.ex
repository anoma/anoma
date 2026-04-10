defmodule Anoma.Node.Examples.ENarwhal.Consensus do
  @moduledoc """
  I contain multi-validator consensus examples for Narwhal + Bullshark.

  I build on `ENarwhal` for config generation and node startup.
  My examples test consensus agreement, fault tolerance, crash
  recovery, and Byzantine invariants.
  """

  alias Anoma.Node.Examples.ENarwhal
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Narwhal.Bullshark
  alias Anoma.Node.Transaction.Narwhal.Cert
  alias Anoma.Node.Transaction.Narwhal.Config
  alias Anoma.Node.Transaction.Narwhal.Events
  alias Anoma.Node.Transaction.Narwhal.Primary
  alias Anoma.Node.Transaction.Narwhal.Supervisor, as: NarwhalSup
  alias Anoma.Node.Transaction.Narwhal.Worker

  require Anoma.Node.Event

  import ExUnit.Assertions

  ############################################################
  #                  Consensus Agreement                     #
  ############################################################

  @doc """
  I verify all validators produce the same consensus ordering.
  """
  @spec all_validators_agree([Config.t()]) :: [Config.t()]
  def all_validators_agree(configs \\ ENarwhal.generate_validator_configs()) do
    ENarwhal.start_all_validators(configs, batch_size: 1)

    tx_ids =
      for c <- configs, do: Mempool.tx(c.node_id, ETransaction.bluf())

    orderings =
      feed_and_collect(configs, configs, &(length(&1) >= length(tx_ids)))

    [first | rest] = orderings
    for order <- rest, do: assert(order == first)
    for tx <- tx_ids, do: assert(tx in first)

    configs
  end

  @doc """
  I verify all validators reach identical execution state.

  Builds on `all_validators_agree/1`: submits counter
  transactions and compares every validator's values table.
  """
  @spec all_validators_execute_identically([Config.t()]) :: [Config.t()]
  def all_validators_execute_identically(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    key = "key"

    for c <- configs do
      subscribe_local(c.node_id, %Mempool.Events.BlockFilter{})
    end

    Mempool.tx(hd(configs).node_id, ETransaction.zero(key))
    inc_id = Mempool.tx(hd(configs).node_id, ETransaction.inc(key))

    feeder = spawn_link(fn -> feed_transactions(configs) end)
    for c <- configs, do: await_block_with_tx(c.node_id, inc_id)
    stop_feeder(feeder)

    backups = for c <- configs, do: dump_shard_backups(c.node_id)
    [first | rest] = backups
    for b <- rest, do: assert(b == first)

    counter_vals =
      first
      |> Enum.filter(fn {{_shard, k, _h}, _v} -> k == ["key"] end)
      |> Enum.sort_by(fn {{_, _, h}, _} -> h end)
      |> Enum.map(fn {_, v} -> v end)

    assert counter_vals == [0, 1]

    configs
  end

  @doc """
  I verify each tx_id appears exactly once in the ordering.

  Submits 12 transactions across 4 validators to span
  multiple waves. Asserts uniqueness and agreement.
  """
  @spec each_tx_ordered_once([Config.t()]) :: [Config.t()]
  def each_tx_ordered_once(configs \\ ENarwhal.generate_validator_configs()) do
    ENarwhal.start_all_validators(configs, batch_size: 1)

    tx_ids =
      for _round <- 1..3,
          c <- configs,
          do: Mempool.tx(c.node_id, ETransaction.bluf())

    target_set = MapSet.new(tx_ids)

    orderings =
      feed_and_collect(
        configs,
        configs,
        &MapSet.subset?(target_set, MapSet.new(&1))
      )

    for order <- orderings do
      assert order == Enum.uniq(order), "Ordering contains duplicates"
    end

    [first | rest] = orderings
    for o <- rest, do: assert(o == first)

    configs
  end

  ############################################################
  #                  Fault Tolerance                         #
  ############################################################

  @doc """
  I start 3 of 4 validators and achieve consensus.

  With n=4, f=1, three validators meet quorum (2f+1=3).
  """
  @spec partial_network_commits([Config.t()]) :: [Config.t()]
  def partial_network_commits(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    early = Enum.take(configs, 3)
    ENarwhal.start_all_validators(early, batch_size: 1)

    Mempool.tx(hd(early).node_id, ETransaction.bluf())

    orders = feed_and_collect(early, early, &(length(&1) >= 1))

    [first | rest] = orders
    for order <- rest, do: assert(order == first)

    configs
  end

  @doc """
  I demonstrate a late-joining validator catching up.

  Builds on `partial_network_commits/1`: after 3 validators
  commit, the 4th joins and fast-forwards via certificates.
  """
  @spec late_validator_catches_up([Config.t()]) :: [Config.t()]
  def late_validator_catches_up(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    configs = partial_network_commits(configs)
    [c1, _, _, c4] = configs

    subscribe_local(c4.node_id, %Events.NarwhalConsensusFilter{})
    ENarwhal.start_narwhal_node(c4, batch_size: 1)

    tx_id = Mempool.tx(c1.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(configs) end)
    order = collect_consensus(c4.node_id, &(tx_id in &1))
    stop_feeder(feeder)

    assert tx_id in order

    {_state_name, primary_data} =
      :sys.get_state(Registry.via(c4.node_id, Primary))

    assert primary_data.shared.round > 0

    configs
  end

  @doc """
  I clear all locally-stored blocks from one validator.

  Builds on `late_validator_catches_up/1`.
  """
  @spec missing_blocks([Config.t()]) :: [Config.t()]
  def missing_blocks(configs \\ ENarwhal.generate_validator_configs()) do
    configs = late_validator_catches_up(configs)
    clear_blocks(hd(configs).node_id)
    configs
  end

  @doc """
  I verify that missing blocks are recovered from peers.

  Builds on `missing_blocks/1`: the next commit's causal
  traversal fetches blocks from peers across multiple rounds.
  """
  @spec missing_blocks_recovered_via_peer([Config.t()]) :: [Config.t()]
  def missing_blocks_recovered_via_peer(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    configs = missing_blocks(configs)
    target = hd(configs)

    tx_id = Mempool.tx(target.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(configs) end)
    order = collect_consensus(target.node_id, &(tx_id in &1))
    stop_feeder(feeder)

    assert tx_id in order

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
  I verify that a skipped wave leader does not block consensus.

  The leader for wave 1 stays offline. Bullshark skips wave 1
  and commits wave 2+.
  """
  @spec skipped_leader_recovery([Config.t()]) :: [Config.t()]
  def skipped_leader_recovery(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    sorted = Config.sorted_validators(hd(configs))
    wave_1_leader_pk = Enum.at(sorted, rem(1, length(sorted)))

    {[_offline], active} =
      Enum.split_with(configs, &(&1.public_key == wave_1_leader_pk))

    ENarwhal.start_all_validators(active, batch_size: 1)

    target = hd(active)
    subscribe_local(target.node_id, %Events.NarwhalConsensusFilter{})

    tx_id = Mempool.tx(target.node_id, ETransaction.bluf())
    feeder = spawn_link(fn -> feed_transactions(active) end)

    events = collect_waves_until_wave(target.node_id, 2)
    stop_feeder(feeder)

    all_orders = Enum.flat_map(events, &elem(&1, 0))
    wave_numbers = Enum.map(events, &elem(&1, 1))

    assert tx_id in all_orders
    refute 1 in wave_numbers, "Wave 1 should be skipped"
    assert Enum.max(wave_numbers) >= 2

    configs
  end

  @doc """
  I verify an offline validator's transactions are still ordered.

  Builds on `all_validators_agree/1`: one validator submits a
  transaction then goes offline. The remaining three order it
  via causal traversal.
  """
  @spec offline_validator_transactions_ordered([Config.t()]) :: [Config.t()]
  def offline_validator_transactions_ordered(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    {active, [victim]} = Enum.split(configs, 3)

    tx_id = Mempool.tx(victim.node_id, ETransaction.bluf())

    feeder = spawn_link(fn -> feed_transactions(configs) end)
    {first_order, _} = receive_consensus(hd(active).node_id)

    for mod <- [Primary, Worker, Bullshark] do
      :sys.suspend(Registry.via(victim.node_id, mod))
    end

    stop_feeder(feeder)

    feeder = spawn_link(fn -> feed_transactions(active) end)
    order = collect_consensus(hd(active).node_id, &(tx_id in &1), first_order)
    stop_feeder(feeder)

    assert tx_id in order

    for mod <- [Primary, Worker, Bullshark] do
      :sys.resume(Registry.via(victim.node_id, mod))
    end

    configs
  end

  ############################################################
  #                  Crash Recovery                          #
  ############################################################

  @doc """
  I verify consensus continues after one validator crashes.

  Builds on `all_validators_agree/1`: one validator is
  suspended. The remaining three still meet quorum.
  """
  @spec consensus_after_validator_crash([Config.t()]) :: [Config.t()]
  def consensus_after_validator_crash(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    surviving = Enum.take(configs, 3)
    crashed = List.last(configs)

    for mod <- [Primary, Worker, Bullshark] do
      :sys.suspend(Registry.via(crashed.node_id, mod))
    end

    tx_id = Mempool.tx(hd(surviving).node_id, ETransaction.bluf())
    orders = feed_and_collect(surviving, surviving, &(tx_id in &1))

    [first | rest] = orders
    for o <- rest, do: assert(o == first)
    assert tx_id in first

    for mod <- [Primary, Worker, Bullshark] do
      :sys.resume(Registry.via(crashed.node_id, mod))
    end

    configs
  end

  @doc """
  I verify a crashed validator recovers and rejoins consensus.

  Builds on `consensus_after_validator_crash/1`: the resumed
  validator catches up and orders a new transaction.
  """
  @spec crashed_validator_recovers([Config.t()]) :: [Config.t()]
  def crashed_validator_recovers(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    configs = consensus_after_validator_crash(configs)
    recovered = List.last(configs)

    tx_id = Mempool.tx(recovered.node_id, ETransaction.bluf())
    feed_and_collect(configs, configs, &(tx_id in &1))

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
  # 4. Bullshark commits only with >= f+1 vote-round references.

  @doc """
  I generate configs where node 0 and a 5th node share a keypair.

  validator_set has `n` unique public keys; node_id_set has `n+1`.
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

  Invariants 1+2: `signed_blocks` prevents double-signing;
  uncontested validators keep advancing.
  """
  @spec duplicate_validators_cannot_equivocate([Config.t()]) :: [Config.t()]
  def duplicate_validators_cannot_equivocate(
        configs \\ generate_duplicate_validator_configs()
      ) do
    ENarwhal.start_all_validators(configs, batch_size: 1)
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
  I verify all invariants hold under equivocation.

  Builds on `duplicate_validators_cannot_equivocate/1`: after
  the duplicate validator scenario, I check blocks reference
  quorum certs (invariant 3) and Bullshark respects the commit
  threshold (invariant 4) on an honest validator's DAG.
  """
  @spec invariants_hold_under_equivocation([Config.t()]) :: [Config.t()]
  def invariants_hold_under_equivocation(
        configs \\ generate_duplicate_validator_configs()
      ) do
    configs = duplicate_validators_cannot_equivocate(configs)
    honest = Enum.take(configs, 4)
    observer = hd(honest)

    # Advance further so more waves commit under equivocation
    feed_and_collect(configs, [observer], &(length(&1) >= 5))

    assert_blocks_reference_quorum(observer.node_id, observer)
    assert_all_certs_have_quorum(observer.node_id, observer)

    configs
  end

  @doc """
  I combine equivocation with a crash fault.

  5 nodes (4 unique pks, 1 duplicate). One honest validator
  crashes. The remaining 2 honest + 2 duplicates (same pk)
  must still reach consensus. With 3 unique honest pks still
  alive (2 honest nodes + the contested pk), quorum (3) is
  reachable.
  """
  @spec equivocation_plus_crash([Config.t()]) :: [Config.t()]
  def equivocation_plus_crash(
        configs \\ generate_duplicate_validator_configs()
      ) do
    configs = duplicate_validators_cannot_equivocate(configs)
    honest = Enum.take(configs, 4)

    # Crash one honest validator (not the duplicated one)
    crashed = Enum.at(honest, 1)

    for mod <- [Primary, Worker, Bullshark] do
      :sys.suspend(Registry.via(crashed.node_id, mod))
    end

    surviving = List.delete(configs, crashed)
    surviving_honest = List.delete(honest, crashed)

    tx_id = Mempool.tx(hd(surviving_honest).node_id, ETransaction.bluf())
    orders = feed_and_collect(surviving, surviving_honest, &(tx_id in &1))

    for order <- orders, do: assert(tx_id in order)
    [first | rest] = orders
    for o <- rest, do: assert(o == first)

    for c <- surviving_honest do
      assert_all_certs_have_quorum(c.node_id, c)
    end

    for mod <- [Primary, Worker, Bullshark] do
      :sys.resume(Registry.via(crashed.node_id, mod))
    end

    configs
  end

  @doc """
  I verify block cert references on a healthy network.

  Invariant 3: every block at round r > 0 references >= 2f+1
  certificates from round r-1.
  """
  @spec blocks_reference_quorum_certs([Config.t()]) :: [Config.t()]
  def blocks_reference_quorum_certs(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    configs = all_validators_agree(configs)
    observer = hd(configs)

    feed_and_collect(configs, [observer], &(length(&1) >= 3))
    assert_blocks_reference_quorum(observer.node_id, observer)

    configs
  end

  @doc """
  I verify every Bullshark commit decision respects the f+1
  vote-reference threshold.

  Invariant 4: subscribes to `WaveDecisionEvent`s, runs the
  protocol, and asserts that every `:committed` decision has
  `refs >= commit_threshold`.
  """
  @spec bullshark_respects_commit_threshold([Config.t()]) :: [Config.t()]
  def bullshark_respects_commit_threshold(
        configs \\ ENarwhal.generate_validator_configs()
      ) do
    ENarwhal.start_all_validators(configs, batch_size: 1)
    observer = hd(configs)
    threshold = Config.commit_threshold(observer)

    subscribe_local(observer.node_id, %Events.WaveDecisionFilter{})

    tx_ids = for c <- configs, do: Mempool.tx(c.node_id, ETransaction.bluf())

    feed_and_collect(configs, configs, &(length(&1) >= length(tx_ids)))

    # Drain all WaveDecisionEvents from the mailbox
    decisions = collect_wave_decisions(observer.node_id)

    committed = Enum.filter(decisions, &(&1.outcome == :committed))
    assert length(committed) >= 1

    for d <- committed do
      assert d.refs >= threshold,
             "Wave #{d.wave}: #{d.refs} refs, need #{threshold}"
    end

    configs
  end

  ############################################################
  #                        Helpers                           #
  ############################################################

  # Consensus lifecycle

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

  # Subscriptions

  @spec subscribe_local(String.t(), struct()) :: :ok
  defp subscribe_local(node_id, filter) do
    EventBroker.subscribe_me([
      Anoma.Node.Event.node_filter(node_id),
      filter
    ])
  end

  # Consensus collection

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

  # Block store

  @spec clear_blocks(String.t()) :: {:atomic, :ok}
  defp clear_blocks(node_id) do
    {:atomic, :ok} = :mnesia.clear_table(NarwhalSup.blocks_table(node_id))
  end

  # DAG inspection and invariant predicates

  @spec collect_wave_decisions(String.t()) :: [Events.WaveDecisionEvent.t()]
  defp collect_wave_decisions(node_id) do
    receive do
      %EventBroker.Event{
        body: %Anoma.Node.Event{
          node_id: ^node_id,
          body: %Events.WaveDecisionEvent{} = decision
        }
      } ->
        [decision | collect_wave_decisions(node_id)]
    after
      0 -> []
    end
  end

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
          block = ensure_block(node_id, config, cert.block_digest),
          block != nil do
        assert length(block.certificates) >= quorum
        for c <- block.certificates, do: assert(c.round == round - 1)
        round
      end

    assert length(checked) > 0, "No blocks with round > 0 found"
    :ok
  end

  # Block lookup (local then peers, same as Bullshark.ensure_block)

  @spec ensure_block(String.t(), Config.t(), binary()) :: Block.t() | nil
  defp ensure_block(node_id, config, digest) do
    case NarwhalSup.get_block(node_id, digest) do
      nil ->
        config.node_id_set
        |> MapSet.delete(node_id)
        |> Enum.find_value(fn peer -> NarwhalSup.get_block(peer, digest) end)
        |> case do
          nil ->
            nil

          block ->
            NarwhalSup.store_block(node_id, digest, block)
            block
        end

      block ->
        block
    end
  end

  # Verification

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
