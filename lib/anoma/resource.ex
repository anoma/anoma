defmodule Anoma.Resource do
  @moduledoc """
  Ι represent a resource
  """

  alias __MODULE__
  use TypedStruct

  typedstruct do
    # TODO Make this an S-Expression
    field(:logic, any(), default: [])
    field(:quantity, integer(), enforce: true)
    field(:value, :binary, default: <<>>)
    # also known as dynamic data
    field(:suffix, :binary, default: <<>>)
    # also known as static data
    field(:prefix, :binary, default: <<>>)
  end

  @spec denomination(t()) :: binary()
  def denomination(denom) do
    Anoma.Serializer.serialize([denom.logic, denom.prefix])
  end
end

defimpl Anoma.Intent, for: Anoma.Resource do
  def is_intent(_data) do
    true
  end
end
