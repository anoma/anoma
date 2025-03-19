defmodule Anoma.Node.Transaction.Mempool.Events do
  @moduledoc """
  I define all events that are being sent by the mempool.

  I also define the filters that can be used to subscribe to these events.
  """

  alias Anoma.Node.Event
  alias Anoma.Node.Transaction.Mempool

  use EventBroker.DefFilter
  use TypedStruct

  ############################################################
  #                           Events                         #
  ############################################################

  typedstruct module: TxEvent do
    @derive Jason.Encoder
    @typedoc """
    I am the type of a transaction event.

    I am sent upon a launch of a transaction, signaling that a specific
    transaction has been launched.

    ### Fileds

    - `:id` - The ID of a launched transaction.
    - `:tx` - The transaction info as stored in Mempool state.
    """

    field(:id, binary())
    field(:tx, Mempool.Tx.t())
  end

  typedstruct module: ConsensusEvent do
    @derive Jason.Encoder
    @typedoc """
    I am the type of a consensus event.

    I am sent upon receiving a consensus, signaling that ordering has been
    assigned to a specific subset of pending transactions.

    ### Fileds

    - `:order` - The list of transaction IDs in apporpriate consensus
                 specified order.
    """

    field(:order, [binary()], default: [])
  end

  typedstruct module: BlockEvent do
    @derive Jason.Encoder
    @typedoc """
    I am the type of a block execition event.

    I am sent upon a completion of all transactions submitted by consensus
    and subsequent creation of a table-backed block.

    ### Fileds

    - `:order` - The consensus info executed, a list of transaction IDs.
    - `:round` - The block number committed.
    """

    field(:order, [binary()], default: [])
    field(:round, non_neg_integer())
  end

  ############################################################
  #                           Filters                        #
  ############################################################

  deffilter TxFilter do
    %EventBroker.Event{body: %Event{body: %TxEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter ConsensusFilter do
    %EventBroker.Event{body: %Event{body: %ConsensusEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter BlockFilter do
    %EventBroker.Event{body: %Event{body: %BlockEvent{}}} ->
      true

    _ ->
      false
  end
end
