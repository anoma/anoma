defmodule Anoma.Node.Transport.Proxy.Events do
  @moduledoc """
  I define all events that are being sent by the backend.

  I also define the filters that can be used to subscribe to these events.
  """
  use EventBroker.DefFilter
  use TypedStruct

  ############################################################
  #                           Events                         #
  ############################################################

  typedstruct enforce: true, module: External do
    @typedoc """
    I hold the content of the Result Event, which conveys the result of
    the transaction candidate code execution on the Anoma VM to
    the Mempool engine.

    ### Fields
    - `:event` - An arbitrary event that is wrapped to be sent over the network.
    """
    field(:event, any())
  end
end
