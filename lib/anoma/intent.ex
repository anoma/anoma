defprotocol Anoma.Intent do
  @moduledoc """
  I am an Intent, I give off the behavior of how Intents can be used.

  Currently there are two kinds of objects which implement me:

  1. Anoma.Resource
  2. Anoma.PartialTx

  More can be found on my API.
  """

  # place holder for now
  @spec is_intent(t()) :: boolean()
  def is_intent(data)
end
