defmodule Anoma.Node.Transaction.Narwhal.Events do
  @moduledoc """
  I define all events and filters for Narwhal consensus.

  I divide events into two categories:

  - **Node-local events**: Subscribe with `NodeFilter` +
    specific filter. These stay within a single node.
  - **Cross-node events**: Subscribe with
    `ValidatorSetFilter` + specific filter. All nodes in the
    validator set see these.

  ### Public API

  Events: `BatchReadyEvent`, `NarwhalConsensusEvent`,
  `WaveDecisionEvent`, `BatchDisseminateEvent`,
  `BlockProposalEvent`, `SignatureEvent`, `CertificateEvent`.

  Filters: `BatchReadyFilter`, `NarwhalConsensusFilter`,
  `WaveDecisionFilter`, `BatchDisseminateFilter`,
  `BlockProposalFilter`, `SignatureFilter`,
  `CertificateFilter`, `ValidatorSetFilter`.
  """

  alias Anoma.Node.Event

  require Anoma.Node.Event

  use EventBroker.DefFilter
  use TypedStruct

  ############################################################
  #                  Node-Local Events                       #
  ############################################################

  typedstruct module: BatchReadyEvent do
    @typedoc """
    I signal that a batch digest is ready for inclusion in a block.

    Published by Worker, consumed by Primary (node-local).
    """
    field(:digest, binary(), default: <<>>)
  end

  typedstruct module: ForeignBatchStoredEvent do
    @typedoc """
    I signal that a foreign batch was stored locally.

    Published by Worker after storing a batch from another
    validator. Primary listens for this to retry signing
    proposals that were missing batch data.
    """
    field(:digest, binary(), default: <<>>)
  end

  typedstruct module: NarwhalConsensusEvent do
    @typedoc """
    I carry the total order of transaction IDs produced by Bullshark.

    Published by Bullshark, consumed by Mempool (node-local).

    ### Fields

    - `:order` - Ordered list of transaction IDs.
    - `:round` - The consensus round that produced this ordering.
    """
    field(:order, [binary()], default: [])
    field(:round, non_neg_integer(), default: 0)
  end

  typedstruct module: WaveDecisionEvent do
    @typedoc """
    I record Bullshark's commit decision for a wave.

    Published by Bullshark for every wave it evaluates. Useful
    for replay auditing and invariant testing: every `:committed`
    decision should have `refs >= commit_threshold`.

    ### Fields

    - `:wave`           - The wave number.
    - `:outcome`        - `:committed` or `:skipped`.
    - `:reason`         - Why: `:committed`, `:no_anchor`,
                          `:insufficient_refs`, or `:unavailable`.
    - `:refs`           - Vote-round references counted (0 when
                          no anchor).
    - `:anchor_digest`  - The anchor cert's block digest, or nil.
    """
    field(:wave, non_neg_integer(), default: 0)
    field(:outcome, :committed | :skipped, default: :skipped)

    field(
      :reason,
      :committed | :no_anchor | :insufficient_refs | :unavailable,
      default: :no_anchor
    )

    field(:refs, non_neg_integer(), default: 0)
    field(:anchor_digest, binary() | nil, default: nil)
  end

  ############################################################
  #                  Cross-Node Events                       #
  ############################################################

  typedstruct module: BatchDisseminateEvent do
    @typedoc """
    I carry a batch of transaction IDs and their code to other
    validators' Workers.

    The `tx_data` field maps each tx_id to its raw Nock code so
    receiving nodes can register and execute the transactions.

    Subscribe WITHOUT NodeFilter to receive from all nodes.
    """
    field(:digest, binary(), default: <<>>)
    field(:tx_ids, [binary()], default: [])
    field(:tx_data, %{binary() => term()}, default: %{})
    field(:from_node, String.t(), default: "")
  end

  typedstruct module: BlockProposalEvent do
    @typedoc """
    I carry a proposed block from a Primary to all other Primaries.

    Subscribe WITHOUT NodeFilter to receive from all nodes.
    """
    field(:block, Anoma.Node.Transaction.Narwhal.Block.t())
    field(:from_node, String.t(), default: "")
  end

  typedstruct module: SignatureEvent do
    @typedoc """
    I carry a validator's signature on a block.

    Subscribe WITHOUT NodeFilter to receive from all nodes.
    """
    field(:block_digest, binary(), default: <<>>)
    field(:signature, binary(), default: <<>>)
    field(:pub_key, binary(), default: <<>>)
    field(:round, non_neg_integer(), default: 0)
    field(:for_creator, binary(), default: <<>>)
    field(:from_node, String.t(), default: "")
  end

  typedstruct module: CertificateEvent do
    @typedoc """
    I carry a completed certificate to all nodes.

    Subscribe WITHOUT NodeFilter to receive from all nodes.
    """
    field(:cert, Anoma.Node.Transaction.Narwhal.Cert.t())
    field(:from_node, String.t(), default: "")
  end

  ############################################################
  #                         Filters                          #
  ############################################################

  # Node-local filters — used with NodeFilter

  deffilter BatchReadyFilter do
    %EventBroker.Event{body: %Event{body: %BatchReadyEvent{}}} -> true
    _ -> false
  end

  deffilter ForeignBatchStoredFilter do
    %EventBroker.Event{body: %Event{body: %ForeignBatchStoredEvent{}}} -> true
    _ -> false
  end

  deffilter NarwhalConsensusFilter do
    %EventBroker.Event{body: %Event{body: %NarwhalConsensusEvent{}}} -> true
    _ -> false
  end

  deffilter WaveDecisionFilter do
    %EventBroker.Event{body: %Event{body: %WaveDecisionEvent{}}} -> true
    _ -> false
  end

  # Cross-node filters — used with ValidatorSetFilter

  deffilter BatchDisseminateFilter do
    %EventBroker.Event{body: %Event{body: %BatchDisseminateEvent{}}} -> true
    _ -> false
  end

  deffilter BlockProposalFilter do
    %EventBroker.Event{body: %Event{body: %BlockProposalEvent{}}} -> true
    _ -> false
  end

  deffilter SignatureFilter, for_creator: <<>> do
    %EventBroker.Event{body: %Event{body: %SignatureEvent{for_creator: c}}} ->
      for_creator == c

    _ ->
      false
  end

  deffilter CertificateFilter do
    %EventBroker.Event{body: %Event{body: %CertificateEvent{}}} -> true
    _ -> false
  end

  # Scopes cross-node subscriptions to a specific validator network.
  # In the in-VM setup all nodes share one EventBroker, so this
  # prevents events from one validator set leaking into another.
  deffilter ValidatorSetFilter, node_ids: MapSet.t() do
    %EventBroker.Event{body: %Event{node_id: id}} ->
      MapSet.member?(node_ids, id)

    _ ->
      false
  end

  ############################################################
  #                    Cross-Node Publish                    #
  ############################################################

  @doc """
  I publish a Narwhal event twice on the local broker: once bare for
  in-VM subscribers, and once wrapped in `Proxy.Events.External`, which
  `Proxy.Node` forwards over the wire (the receiving VM's gRPC `PubSub`
  unwraps and re-fires the bare event). This is a shim over the generic
  `Proxy.Node` filter, pending a per-engine Narwhal proxy.
  """
  @spec publish_cross_node(String.t(), struct()) :: :ok
  def publish_cross_node(node_id, body) do
    EventBroker.event(Anoma.Node.Event.new_with_body(node_id, body))

    external_body =
      %Anoma.Node.Transport.Proxy.Events.External{event: body}

    EventBroker.event(Anoma.Node.Event.new_with_body(node_id, external_body))

    :ok
  end
end
