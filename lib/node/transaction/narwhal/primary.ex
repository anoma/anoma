defmodule Anoma.Node.Transaction.Narwhal.Primary do
  @moduledoc """
  I am the Narwhal Primary.

  I run a gen_statem with two states:

  - `:block_creation` -- Collecting batch digests and
    certificates from round r-1. When I have >= quorum certs,
    I create a block, sign it, and publish a
    BlockProposalEvent, then transition to
    `:signature_collection`.

  - `:signature_collection` -- Collecting signatures on my
    proposed block. When I have >= quorum signatures, I form a
    Certificate, publish a CertificateEvent, and transition
    back to `:block_creation` for round r+1.

  In both states I sign other validators' block proposals.

  ### State Data

  Each state carries a `Shared` struct (DAG, round, config)
  plus state-specific accumulators.
  """

  alias Anoma.Node.Registry

  alias Anoma.Node.Transaction.Narwhal.Block
  alias Anoma.Node.Transaction.Narwhal.Cert
  alias Anoma.Node.Transaction.Narwhal.Config
  alias Anoma.Node.Transaction.Narwhal.Events
  alias Anoma.Node.Transaction.Narwhal.Supervisor, as: NarwhalSup
  alias Anoma.Node.Transaction.Narwhal.Worker

  require Anoma.Node.Event

  use TypedStruct

  @behaviour :gen_statem

  ############################################################
  #                    State Data Types                      #
  ############################################################

  @type startup_options :: {:node_id, String.t()} | {:config, Config.t()}

  typedstruct module: Shared do
    @moduledoc false
    field(:node_id, String.t(), enforce: true)
    field(:config, Config.t(), enforce: true)
    field(:dag, %{{binary(), non_neg_integer()} => Cert.t()}, default: %{})
    field(:round, non_neg_integer(), default: 0)

    field(:signed_blocks, MapSet.t({binary(), non_neg_integer()}),
      default: MapSet.new()
    )

    field(:pending_proposals, [{Block.t(), String.t()}], default: [])
  end

  typedstruct module: BlockCreation do
    @moduledoc false
    field(:shared, Shared.t(), enforce: true)
    field(:round_certs, [Cert.t()], default: [])
    field(:batch_digests, [binary()], default: [])
  end

  typedstruct module: SignatureCollection do
    @moduledoc false
    field(:shared, Shared.t(), enforce: true)
    field(:current_block, Block.t(), enforce: true)
    field(:current_cert, Cert.t(), enforce: true)
    field(:buffered_digests, [binary()], default: [])
  end

  ############################################################
  #                   gen_statem Behavior                    #
  ############################################################

  @spec child_spec(list(startup_options())) :: Supervisor.child_spec()
  def child_spec(args) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, [args]}, type: :worker}
  end

  @spec start_link(list(startup_options())) :: :gen_statem.start_ret()
  def start_link(args) do
    node_id = Keyword.fetch!(args, :node_id)

    :gen_statem.start_link(
      Registry.via(node_id, __MODULE__),
      __MODULE__,
      args,
      []
    )
  end

  @impl true
  def callback_mode(), do: :state_functions

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    node_id = Keyword.fetch!(args, :node_id)
    config = Keyword.fetch!(args, :config)

    vs_filter = %Events.ValidatorSetFilter{node_ids: config.node_id_set}

    EventBroker.subscribe_me([
      Anoma.Node.Event.node_filter(node_id),
      %Events.BatchReadyFilter{}
    ])

    EventBroker.subscribe_me([vs_filter, %Events.BlockProposalFilter{}])

    EventBroker.subscribe_me([
      vs_filter,
      %Events.SignatureFilter{for_creator: config.public_key}
    ])

    EventBroker.subscribe_me([vs_filter, %Events.CertificateFilter{}])

    EventBroker.subscribe_me([
      Anoma.Node.Event.node_filter(node_id),
      %Events.ForeignBatchStoredFilter{}
    ])

    shared = %Shared{node_id: node_id, config: config}
    {:ok, :block_creation, %BlockCreation{shared: shared}}
  end

  ############################################################
  #               :block_creation state                      #
  ############################################################

  @spec block_creation(:gen_statem.event_type(), term(), BlockCreation.t()) ::
          :gen_statem.state_function_result()
  def block_creation(
        :info,
        %EventBroker.Event{body: %Anoma.Node.Event{body: body}},
        data
      ) do
    handle_block_creation(body, data)
  end

  def block_creation(:info, _event, data), do: {:keep_state, data}

  defp handle_block_creation(%Events.BatchReadyEvent{digest: digest}, data) do
    data = %{data | batch_digests: [digest | data.batch_digests]}

    if should_create_block?(data) do
      create_and_propose_block(data)
    else
      {:keep_state, data}
    end
  end

  defp handle_block_creation(%Events.CertificateEvent{cert: cert}, data) do
    if Cert.valid_for_config?(cert, data.shared.config) do
      data = store_incoming_cert(cert, data)

      cond do
        should_create_block?(data) ->
          create_and_propose_block(data)

        cert.round >= data.shared.round ->
          maybe_advance_block_creation(data)

        true ->
          {:keep_state, data}
      end
    else
      {:keep_state, data}
    end
  end

  defp handle_block_creation(
         %Events.BlockProposalEvent{block: block, from_node: from},
         data
       ) do
    {:keep_state, sign_block_if_valid(block, from, data)}
  end

  defp handle_block_creation(%Events.ForeignBatchStoredEvent{}, data) do
    {:keep_state, retry_pending_proposals(data)}
  end

  defp handle_block_creation(_body, data), do: {:keep_state, data}

  ############################################################
  #            :signature_collection state                   #
  ############################################################

  @spec signature_collection(
          :gen_statem.event_type(),
          term(),
          SignatureCollection.t()
        ) ::
          :gen_statem.state_function_result()
  def signature_collection(
        :info,
        %EventBroker.Event{body: %Anoma.Node.Event{body: body}},
        data
      ) do
    handle_signature_collection(body, data)
  end

  def signature_collection(:info, _event, data), do: {:keep_state, data}

  defp handle_signature_collection(
         %Events.SignatureEvent{
           block_digest: bd,
           signature: signature,
           pub_key: pub_key,
           round: round
         },
         data = %{
           current_cert: %{block_digest: bd},
           shared: %{round: round, config: config}
         }
       ) do
    if MapSet.member?(config.validator_set, pub_key) and
         Anoma.Crypto.Sign.verify_detached(signature, bd, pub_key) do
      cert = Cert.add_signature(data.current_cert, pub_key, signature)
      data = %{data | current_cert: cert}

      if Cert.has_quorum?(cert, Config.quorum(config)) do
        publish_certificate_and_advance(data)
      else
        {:keep_state, data}
      end
    else
      {:keep_state, data}
    end
  end

  defp handle_signature_collection(%Events.SignatureEvent{}, data) do
    {:keep_state, data}
  end

  defp handle_signature_collection(
         %Events.BatchReadyEvent{digest: digest},
         data
       ) do
    {:keep_state,
     %{data | buffered_digests: [digest | data.buffered_digests]}}
  end

  defp handle_signature_collection(
         %Events.BlockProposalEvent{block: block, from_node: from},
         data
       ) do
    {:keep_state, sign_block_if_valid(block, from, data)}
  end

  defp handle_signature_collection(
         %Events.CertificateEvent{cert: cert},
         data = %{shared: shared}
       ) do
    if Cert.valid_for_config?(cert, shared.config) do
      new_dag = Map.put_new(shared.dag, {cert.validator, cert.round}, cert)
      new_shared = %{shared | dag: new_dag}

      if cert.round > shared.round do
        maybe_advance_from_signature(data, new_shared)
      else
        {:keep_state, %{data | shared: new_shared}}
      end
    else
      {:keep_state, data}
    end
  end

  defp handle_signature_collection(%Events.ForeignBatchStoredEvent{}, data) do
    {:keep_state, retry_pending_proposals(data)}
  end

  defp handle_signature_collection(_body, data), do: {:keep_state, data}

  ############################################################
  #                    Implementation                        #
  ############################################################

  defp publish(node_id, body) do
    EventBroker.event(Anoma.Node.Event.new_with_body(node_id, body))
  end

  # Fast-forward: find the highest round >= min_round where we have
  # quorum unique certs. Returns {:advance, new_round, round_certs}
  # or :current if no fast-forward is possible.
  @spec maybe_fast_forward(Shared.t(), non_neg_integer()) ::
          {:advance, non_neg_integer(), [Cert.t()]} | :current
  defp maybe_fast_forward(shared, min_round) do
    quorum = Config.quorum(shared.config)

    shared.dag
    |> Enum.group_by(
      fn {{_v, round}, _} -> round end,
      fn {_, cert} -> cert end
    )
    |> Enum.filter(fn {round, certs} ->
      round >= min_round and unique_cert_count(certs) >= quorum
    end)
    |> Enum.max_by(&elem(&1, 0), fn -> nil end)
    |> case do
      nil -> :current
      {round, certs} -> {:advance, round + 1, certs}
    end
  end

  defp maybe_advance_block_creation(data) do
    case maybe_fast_forward(data.shared, data.shared.round) do
      :current ->
        {:keep_state, data}

      {:advance, new_round, round_certs} ->
        data = %{
          data
          | shared: %{data.shared | round: new_round},
            round_certs: round_certs
        }

        data = retry_pending_proposals(data)

        if should_create_block?(data) do
          create_and_propose_block(data)
        else
          {:keep_state, data}
        end
    end
  end

  defp maybe_advance_from_signature(data, new_shared) do
    case maybe_fast_forward(new_shared, new_shared.round + 1) do
      :current ->
        {:keep_state, %{data | shared: new_shared}}

      {:advance, new_round, round_certs} ->
        new_data = %BlockCreation{
          shared: %{new_shared | round: new_round},
          round_certs: round_certs,
          batch_digests: data.buffered_digests
        }

        new_data = retry_pending_proposals(new_data)

        if should_create_block?(new_data) do
          create_and_propose_block(new_data)
        else
          {:next_state, :block_creation, new_data}
        end
    end
  end

  @spec store_incoming_cert(Cert.t(), BlockCreation.t()) :: BlockCreation.t()
  defp store_incoming_cert(cert, data = %{shared: shared}) do
    key = {cert.validator, cert.round}

    if Map.has_key?(shared.dag, key) do
      data
    else
      shared = %{shared | dag: Map.put(shared.dag, key, cert)}
      data = %{data | shared: shared}

      if shared.round > 0 and cert.round == shared.round - 1 do
        %{data | round_certs: [cert | data.round_certs]}
      else
        data
      end
    end
  end

  @spec should_create_block?(BlockCreation.t()) :: boolean()
  defp should_create_block?(%{batch_digests: []}), do: false
  defp should_create_block?(%{shared: %{round: 0}}), do: true

  defp should_create_block?(data) do
    unique_cert_count(data.round_certs) >= Config.quorum(data.shared.config)
  end

  defp unique_cert_count(certs) do
    certs |> MapSet.new(& &1.validator) |> MapSet.size()
  end

  @spec create_and_propose_block(BlockCreation.t()) ::
          :gen_statem.state_function_result()
  defp create_and_propose_block(%BlockCreation{
         shared: shared = %Shared{config: config, round: round, node_id: nid},
         round_certs: round_certs,
         batch_digests: batch_digests
       }) do
    block =
      %Block{
        batch_digests: Enum.reverse(batch_digests),
        certificates: round_certs,
        round: round,
        creator: config.public_key
      }
      |> Block.sign(config.secret_key)

    digest = Block.digest(block)
    NarwhalSup.store_block(nid, digest, block)

    our_sig = Anoma.Crypto.Sign.sign_detached(digest, config.secret_key)

    cert =
      %Cert{block_digest: digest, validator: config.public_key, round: round}
      |> Cert.add_signature(config.public_key, our_sig)

    publish(nid, %Events.BlockProposalEvent{block: block, from_node: nid})

    new_data = %SignatureCollection{
      shared: shared,
      current_block: block,
      current_cert: cert
    }

    if Cert.has_quorum?(cert, Config.quorum(config)) do
      publish_certificate_and_advance(new_data)
    else
      {:next_state, :signature_collection, new_data}
    end
  end

  @spec publish_certificate_and_advance(SignatureCollection.t()) ::
          :gen_statem.state_function_result()
  defp publish_certificate_and_advance(%SignatureCollection{
         shared: shared = %Shared{dag: dag, round: round, node_id: nid},
         current_cert: cert,
         buffered_digests: buffered
       }) do
    new_dag = Map.put(dag, {cert.validator, cert.round}, cert)

    publish(nid, %Events.CertificateEvent{
      cert: cert,
      from_node: nid
    })

    new_shared = %{
      shared
      | dag: new_dag,
        round: round + 1
    }

    round_certs = for {{_v, r}, c} <- new_dag, r == round, do: c

    new_data = %BlockCreation{
      shared: new_shared,
      round_certs: round_certs,
      batch_digests: buffered
    }

    new_data = retry_pending_proposals(new_data)

    if unique_cert_count(round_certs) >= Config.quorum(new_shared.config) and
         buffered != [] do
      create_and_propose_block(new_data)
    else
      {:next_state, :block_creation, new_data}
    end
  end

  @spec sign_block_if_valid(
          Block.t(),
          String.t(),
          BlockCreation.t() | SignatureCollection.t()
        ) ::
          BlockCreation.t() | SignatureCollection.t()
  defp sign_block_if_valid(_block, from, data = %{shared: %{node_id: from}}),
    do: data

  defp sign_block_if_valid(
         block,
         from_node,
         data = %{shared: shared = %{config: config}}
       ) do
    cond do
      MapSet.member?(shared.signed_blocks, {block.creator, block.round}) ->
        data

      not Block.valid?(block) or
        not MapSet.member?(config.validator_set, block.creator) or
          not valid_block_certs?(block, config) ->
        data

      block.round > shared.round or
          not Worker.has_batches?(shared.node_id, block.batch_digests) ->
        pending = [{block, from_node} | shared.pending_proposals]
        %{data | shared: %{shared | pending_proposals: pending}}

      true ->
        block_digest = Block.digest(block)
        NarwhalSup.store_block(shared.node_id, block_digest, block)

        sig = Anoma.Crypto.Sign.sign_detached(block_digest, config.secret_key)

        publish(shared.node_id, %Events.SignatureEvent{
          block_digest: block_digest,
          signature: sig,
          pub_key: config.public_key,
          round: block.round,
          for_creator: block.creator,
          from_node: shared.node_id
        })

        new_signed =
          MapSet.put(shared.signed_blocks, {block.creator, block.round})

        %{data | shared: %{shared | signed_blocks: new_signed}}
    end
  end

  @spec valid_block_certs?(Block.t(), Config.t()) :: boolean()
  defp valid_block_certs?(%{round: 0, certificates: []}, _config), do: true
  defp valid_block_certs?(%{round: 0}, _config), do: false

  defp valid_block_certs?(block, config) do
    Enum.all?(block.certificates, fn cert ->
      cert.round == block.round - 1 and Cert.valid_for_config?(cert, config)
    end)
  end

  @spec retry_pending_proposals(BlockCreation.t() | SignatureCollection.t()) ::
          BlockCreation.t() | SignatureCollection.t()
  defp retry_pending_proposals(data = %{shared: shared}) do
    data = %{data | shared: %{shared | pending_proposals: []}}

    Enum.reduce(shared.pending_proposals, data, fn {block, from}, acc ->
      sign_block_if_valid(block, from, acc)
    end)
  end
end
