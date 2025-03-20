defmodule Anoma.Node.Intents.IntentPool.Events do
  @moduledoc """
  I define all events that are being sent by the intent pool.

  I also define the filters that can be used to subscribe to these events.
  """

  alias Anoma.Node.Event
  alias Anoma.RM.Intent

  use EventBroker.DefFilter
  use TypedStruct

  ############################################################
  #                           Events                         #
  ############################################################

  typedstruct enforce: true, module: IntentAddSuccess do
    @typedoc """
    I am an event specifying that an intent has been submitted succesfully.

    ### Fields
    - `:intent` - The intent added.
    """
    field(:intent, Intent.t())
  end

  typedstruct enforce: true, module: IntentAddError do
    @typedoc """
    I am an event specifying that an intent submission has failed alongside
    with a reason.

    ### Fields
    - `:intent` - The intent submitted.
    - `:reason` - The reason why it was rejected from the pool.
    """
    field(:intent, Intent.t())
    field(:reason, String.t())
  end

  ############################################################
  #                           Filters                        #
  ############################################################

  deffilter IntentAddSuccessFilter do
    %EventBroker.Event{
      body: %Event{body: %IntentAddSuccess{}}
    } ->
      true

    _ ->
      false
  end

  deffilter IntentAddErrorFilter do
    %EventBroker.Event{body: %Event{body: %IntentAddError{}}} ->
      true

    _ ->
      false
  end
end
