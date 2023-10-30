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
    field(:value, binary(), default: <<>>)
    # also known as dynamic data
    field(:suffix, binary(), default: <<>>)
    # also known as static data
    field(:prefix, binary(), default: <<>>)
    field(:data, any(), default: <<>>)
  end

  @spec denomination(t()) :: binary()
  def denomination(denom) do
    Anoma.Serializer.serialize([denom.logic, denom.prefix])
  end

  @doc """
  Create an empty resource with a given quantity
  """
  @spec new(integer()) :: t()
  def new(num) do
    %Resource{quantity: num}
  end

  @doc """

  I help create a completely empty resource, with a given term as the
  suffix.

  This is mainly helpful in testing, as we can create unique empty
  resources.

  ## Parameters

    - `suffix` - any term that will be turned into a binary for testing

  ## Output

    - The empty resource

  """
  @spec make_empty(term()) :: t()
  def make_empty(suffix) do
    %Resource{quantity: 0, suffix: :erlang.term_to_binary(suffix)}
  end
end

defimpl Anoma.Intent, for: Anoma.Resource do
  def is_intent(_data) do
    true
  end
end
