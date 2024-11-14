defprotocol Anoma.RM.Intent do
  @moduledoc """
  I am the Intent protocol.
  """

  @spec compose(t(), t()) :: t()
  def compose(intent_1, intent_2)

  @spec verify(t()) :: true | {:error, any()}
  def verify(intent)
end

defimpl Anoma.RM.Intent, for: Anoma.RM.DumbIntent do
  alias Anoma.RM.DumbIntent

  @impl true
  def compose(intent_1 = %DumbIntent{}, intent_2 = %DumbIntent{}) do
    %DumbIntent{value: intent_1.value + intent_2.value}
  end

  @impl true
  def verify(intent = %DumbIntent{}) do
    intent.value == 0
  end
end
