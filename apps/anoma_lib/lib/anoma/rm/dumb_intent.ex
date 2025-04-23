defmodule Anoma.RM.DumbIntent do
  use TypedStruct

  typedstruct do
    @derive Jason.Encoder
    @typedoc """
    I hold the state for an intent.

    ### Fields
    - `:value` - The value of the intent. An ineger value between -infinity and infinity.
    """
    field(:value, integer(), default: 0)
  end
end
