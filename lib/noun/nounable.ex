defprotocol Noun.Nounable do
  @doc """
  I turn the transaction into a noun
  """
  @spec to_noun(t()) :: Noun.t()
  def to_noun(transaction)
end

defmodule Noun.Nounable.Kind do
  @doc """
  I convert the given `t:Noun.t/0` into the given structure
  """
  @callback from_noun(noun :: Noun.t()) :: {:ok, any()} | :error
end
