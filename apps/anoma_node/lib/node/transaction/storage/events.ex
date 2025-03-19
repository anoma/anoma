defmodule Anoma.Node.Transaction.Storage.Events do
  @moduledoc """
  I define all events that are being sent by the storage engine.

  I also define the filters that can be used to subscribe to these events.
  """

  alias Anoma.Node.Event

  use EventBroker.DefFilter
  use TypedStruct

  ############################################################
  #                           Events                         #
  ############################################################

  typedstruct enforce: true, module: WriteEvent do
    @typedoc """
    I am the type of a write event.

    I am sent whenever something has been written at a particular height.

    ### Fields

    - `:height` - The height at which something was just written.
    - `:writes` - A list of tuples {key, value}
    """

    field(:height, non_neg_integer())
    field(:writes, list({Anoma.Node.Transaction.Storage.bare_key(), term()}))
  end

  ############################################################
  #                           Filters                        #
  ############################################################

  deffilter HeightFilter, height: non_neg_integer() do
    %EventBroker.Event{body: %Event{body: %{height: ^height}}} -> true
    _ -> false
  end
end
