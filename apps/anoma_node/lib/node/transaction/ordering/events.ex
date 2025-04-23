defmodule Anoma.Node.Transaction.Ordering.Events do
  @moduledoc """
  I define the events and event filters for the ordering engine.
  """
  alias Anoma.Node.Event

  use EventBroker.DefFilter
  use TypedStruct

  use EventBroker.DefFilter

  ############################################################
  #                           Events                         #
  ############################################################
  typedstruct enforce: true, module: OrderEvent do
    @derive Jason.Encoder
    @typedoc """
    I am the type of an ordering Event.

    I am sent whenever the transaction with which I am associated gets a
    global timestamp.

    ### Fields

    - `tx_id` - The ID of the transaction which was ordered.
    """

    field(:tx_id, binary())
  end

  ############################################################
  #                           Filters                        #
  ############################################################

  deffilter TxIdFilter, tx_id: binary() do
    %EventBroker.Event{body: %Event{body: %{tx_id: ^tx_id}}} -> true
    _ -> false
  end
end
