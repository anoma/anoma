defmodule Anoma.Node.Transaction.Narwhal.Bullshark do
  @moduledoc """
  I am the Bullshark consensus module.

  I subscribe to CertificateEvents from the validator network and
  maintain a local DAG view. Every 2 rounds (one "wave"), I check
  if the anchor leader has enough references from the vote round.
  On commit, I traverse the causal history to produce a total
  order of transaction IDs.

  ### Public API

  - `wave_leader/2`  - Return the leader for a given wave.

  ### Bullshark Protocol

  - **Wave** = 2 rounds (anchor round + vote round)
  - **Anchor round** = wave_number * 2 (even rounds)
  - **Vote round** = wave_number * 2 + 1 (odd rounds)
  - **Leader** = round-robin over sorted validator set
  - **Commit rule**: anchor has >= f+1 vote-round refs
  - **Transitive commits**: the DFS causal traversal from a
    committed anchor naturally orders all reachable certs,
    including earlier anchors in the causal history
  """

  alias Anoma.Node.Logging
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Narwhal.Block
  alias Anoma.Node.Transaction.Narwhal.Cert
  alias Anoma.Node.Transaction.Narwhal.Config
  alias Anoma.Node.Transaction.Narwhal.Events
  alias Anoma.Node.Transaction.Narwhal.Supervisor, as: NarwhalSup
  alias Anoma.Node.Transaction.Narwhal.Worker

  require Anoma.Node.Event

  use GenServer
  use TypedStruct

  ############################################################
  #                         State                            #
  ############################################################

  @type startup_options :: {:node_id, String.t()} | {:config, Config.t()}

  typedstruct do
    field(:node_id, String.t(), enforce: true)
    field(:config, Config.t(), enforce: true)
    field(:dag, %{{binary(), non_neg_integer()} => Cert.t()}, default: %{})
    field(:last_committed_wave, integer(), default: -1)
    field(:committed_set, MapSet.t(binary()), default: MapSet.new())
  end

  ############################################################
  #                    Public API                            #
  ############################################################

  @spec wave_leader(Config.t(), non_neg_integer()) :: binary()
  def wave_leader(config, wave) do
    validators = Config.sorted_validators(config)
    Enum.at(validators, rem(wave, length(validators)))
  end

  ############################################################
  #                   GenServer Behavior                     #
  ############################################################

  @spec start_link(list(startup_options())) :: GenServer.on_start()
  def start_link(args) do
    node_id = Keyword.fetch!(args, :node_id)

    GenServer.start_link(__MODULE__, args,
      name: Registry.via(node_id, __MODULE__)
    )
  end

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    node_id = Keyword.fetch!(args, :node_id)
    config = Keyword.fetch!(args, :config)

    EventBroker.subscribe_me([
      %Events.ValidatorSetFilter{node_ids: config.node_id_set},
      %Events.CertificateFilter{}
    ])

    {:ok, %__MODULE__{node_id: node_id, config: config}}
  end

  @impl true
  def handle_info(
        %EventBroker.Event{
          body: %Anoma.Node.Event{
            body: %Events.CertificateEvent{cert: cert}
          }
        },
        state
      ) do
    if Cert.valid_for_config?(cert, state.config) do
      dag = Map.put_new(state.dag, {cert.validator, cert.round}, cert)
      {:noreply, try_commit(%{state | dag: dag})}
    else
      {:noreply, state}
    end
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  ############################################################
  #                      Implementation                     #
  ############################################################

  @spec try_commit(t()) :: t()
  defp try_commit(state) do
    max_round =
      state.dag
      |> Map.keys()
      |> Enum.map(&elem(&1, 1))
      |> Enum.max(fn -> -1 end)

    max_wave = div(max_round, 2)

    if max_wave <= state.last_committed_wave do
      state
    else
      Enum.reduce_while(
        (state.last_committed_wave + 1)..max_wave//1,
        state,
        fn wave, acc ->
          case try_commit_wave(acc, wave) do
            {:ok, new_state} -> {:cont, new_state}
            :unavailable -> {:halt, acc}
          end
        end
      )
    end
  end

  @spec try_commit_wave(t(), non_neg_integer()) :: {:ok, t()} | :unavailable
  defp try_commit_wave(state, wave) do
    anchor_round = wave * 2
    vote_round = wave * 2 + 1
    leader = wave_leader(state.config, wave)

    case Map.get(state.dag, {leader, anchor_round}) do
      nil ->
        publish_wave_decision(state, wave, :skipped, :no_anchor, 0, nil)
        {:ok, state}

      anchor_cert ->
        refs = count_references(state, anchor_cert, vote_round)
        digest = anchor_cert.block_digest

        if refs >= Config.commit_threshold(state.config) do
          case commit_anchor(state, anchor_cert, wave) do
            {:ok, new_state} ->
              publish_wave_decision(state, wave, :committed, :committed, refs, digest)
              {:ok, new_state}

            :unavailable ->
              :unavailable
          end
        else
          publish_wave_decision(state, wave, :skipped, :insufficient_refs, refs, digest)
          {:ok, state}
        end
    end
  end

  @spec count_references(t(), Cert.t(), non_neg_integer()) ::
          non_neg_integer()
  defp count_references(state, anchor_cert, vote_round) do
    Enum.count(Config.sorted_validators(state.config), fn validator ->
      references_anchor?(state, validator, vote_round, anchor_cert)
    end)
  end

  @spec references_anchor?(t(), binary(), non_neg_integer(), Cert.t()) ::
          boolean()
  defp references_anchor?(state, validator, round, anchor_cert) do
    with %Cert{} = vote_cert <- Map.get(state.dag, {validator, round}),
         {:ok, block} <- ensure_block(state, vote_cert.block_digest) do
      Enum.any?(block.certificates, fn prev_cert ->
        prev_cert.block_digest == anchor_cert.block_digest and
          prev_cert.validator == anchor_cert.validator
      end)
    else
      _ -> false
    end
  end

  @spec commit_anchor(t(), Cert.t(), non_neg_integer()) ::
          {:ok, t()} | :unavailable
  defp commit_anchor(state, anchor_cert, wave) do
    case traverse_causal_history(state, [anchor_cert], state.committed_set) do
      :unavailable ->
        Logging.log_event(
          state.node_id,
          :warning,
          "Block data unavailable, deferring wave #{wave}"
        )

        :unavailable

      {:ok, ordered_certs, committed} ->
        commit_ordered(state, ordered_certs, committed, wave)
    end
  end

  @spec commit_ordered(t(), [Cert.t()], MapSet.t(binary()), non_neg_integer()) ::
          {:ok, t()} | :unavailable
  defp commit_ordered(state, ordered_certs, committed, wave) do
    case resolve_tx_ids(state, ordered_certs) do
      :unavailable ->
        Logging.log_event(
          state.node_id,
          :warning,
          "Batch data unavailable, deferring wave #{wave}"
        )

        :unavailable

      {:ok, tx_ids} ->
        if tx_ids != [] do
          EventBroker.event(
            Anoma.Node.Event.new_with_body(
              state.node_id,
              %Events.NarwhalConsensusEvent{order: tx_ids, round: wave}
            )
          )
        end

        {:ok, %{state | last_committed_wave: wave, committed_set: committed}}
    end
  end

  @spec traverse_causal_history(t(), [Cert.t()], MapSet.t()) ::
          {:ok, [Cert.t()], MapSet.t()} | :unavailable
  defp traverse_causal_history(state, certs, committed) do
    case do_traverse(state, certs, committed, []) do
      {:ok, acc, committed} -> {:ok, Enum.reverse(acc), committed}
      :unavailable -> :unavailable
    end
  end

  defp do_traverse(_state, [], committed, acc), do: {:ok, acc, committed}

  defp do_traverse(state, [cert | rest], committed, acc) do
    if MapSet.member?(committed, cert.block_digest) do
      do_traverse(state, rest, committed, acc)
    else
      new_committed = MapSet.put(committed, cert.block_digest)

      case ensure_block(state, cert.block_digest) do
        :unavailable ->
          :unavailable

        {:ok, block} ->
          prev_certs =
            block.certificates
            |> Enum.map(fn ref ->
              Map.get(state.dag, {ref.validator, ref.round}, ref)
            end)
            |> Enum.reject(fn prev ->
              MapSet.member?(new_committed, prev.block_digest)
            end)

          case do_traverse(state, prev_certs, new_committed, acc) do
            {:ok, acc, newer_committed} ->
              do_traverse(state, rest, newer_committed, [cert | acc])

            :unavailable ->
              :unavailable
          end
      end
    end
  end

  @spec resolve_tx_ids(t(), [Cert.t()]) :: {:ok, [binary()]} | :unavailable
  defp resolve_tx_ids(%{node_id: node_id, config: config}, certs) do
    peer_ids =
      config.node_id_set |> MapSet.delete(node_id) |> MapSet.to_list()

    digests =
      Enum.flat_map(certs, fn cert ->
        case NarwhalSup.get_block(node_id, cert.block_digest) do
          nil -> []
          block -> block.batch_digests
        end
      end)

    result =
      Enum.reduce_while(digests, {:ok, []}, fn digest, {:ok, acc} ->
        case Worker.get_batch(node_id, digest) do
          {:ok, tx_ids} ->
            {:cont, {:ok, [tx_ids | acc]}}

          :absent ->
            case fetch_from_peers(node_id, digest, peer_ids) do
              {:ok, tx_ids} -> {:cont, {:ok, [tx_ids | acc]}}
              :unavailable -> {:halt, :unavailable}
            end
        end
      end)

    case result do
      {:ok, chunks} -> {:ok, chunks |> Enum.reverse() |> List.flatten()}
      :unavailable -> :unavailable
    end
  end

  @spec ensure_block(t(), binary()) :: {:ok, Block.t()} | :unavailable
  defp ensure_block(%{node_id: node_id, config: config}, block_digest) do
    case NarwhalSup.get_block(node_id, block_digest) do
      nil ->
        peer_ids =
          config.node_id_set |> MapSet.delete(node_id) |> MapSet.to_list()

        Enum.find_value(peer_ids, :unavailable, fn peer_id ->
          case NarwhalSup.get_block(peer_id, block_digest) do
            nil ->
              nil

            block ->
              NarwhalSup.store_block(node_id, block_digest, block)
              {:ok, block}
          end
        end)

      block ->
        {:ok, block}
    end
  end

  @spec fetch_from_peers(String.t(), binary(), [String.t()]) ::
          {:ok, [binary()]} | :unavailable
  defp fetch_from_peers(node_id, digest, peer_ids) do
    Enum.find_value(peer_ids, fn peer_id ->
      case Worker.get_full_batch(peer_id, digest) do
        {:ok, tx_ids, tx_data} ->
          case Worker.store_fetched_batch(node_id, digest, tx_ids, tx_data) do
            :ok -> {:ok, tx_ids}
            :rejected -> nil
          end

        :absent ->
          nil
      end
    end) || :unavailable
  end

  @spec publish_wave_decision(
          t(),
          non_neg_integer(),
          :committed | :skipped,
          :committed | :no_anchor | :insufficient_refs,
          non_neg_integer(),
          binary() | nil
        ) :: term()
  defp publish_wave_decision(state, wave, outcome, reason, refs, anchor_digest) do
    EventBroker.event(
      Anoma.Node.Event.new_with_body(
        state.node_id,
        %Events.WaveDecisionEvent{
          wave: wave,
          outcome: outcome,
          reason: reason,
          refs: refs,
          anchor_digest: anchor_digest
        }
      )
    )
  end
end
