defmodule Anoma.Node.Examples.DumbTransaction do
  use TypedStruct

  typedstruct do
    @typedoc """
    I hold the state for an intent.

    ### Fields
    - `:value` - The value of the intent. An ineger value between -infinity and infinity.
    """
    field(:value, integer(), default: 0)
  end
end

defimpl Anoma.RM.Transaction, for: Anoma.Node.Examples.DumbTransaction do
  alias Anoma.Node.Examples.DumbTransaction

  @impl true
  def compose(intent_1 = %DumbTransaction{}, intent_2 = %DumbTransaction{}) do
    %DumbTransaction{value: intent_1.value + intent_2.value}
  end

  @impl true
  def verify(intent = %DumbTransaction{}) do
    intent.value == 0
  end

  @impl true
  def nullifiers(%DumbTransaction{}) do
    MapSet.new()
  end

  @impl true
  def commitments(%DumbTransaction{}) do
    MapSet.new()
  end

  @impl true
  def compose_pre_check(_, _), do: true
end
