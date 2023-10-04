defmodule Anoma.Resource do
  @moduledoc """
  Ι represent a resource
  """

  alias __MODULE__
  use TypedStruct

  typedstruct do
    # TODO Should we make this a sexp or a logic?
    field(:logic, Anoma.Logic.t(), default: 0)
    field(:quantity, integer(), enforce: true)
    field(:value, :binary, default: <<>>)
    # also known as dynamic data
    field(:suffix, :binary, default: <<>>)
    # also known as static data
    field(:prefix, :binary, default: <<>>)
    field(:data, any(), default: <<>>)
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
