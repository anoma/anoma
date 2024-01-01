defmodule Anoma.Block.Base do
  @moduledoc """

  I represent the Base part of a block.

  ### Type

     - `:transactions` - A list of transactions.

  """
  use TypedStruct

  alias Anoma.Block.Base

  typedstruct do
    field(:transactions, list(any()), default: [])
  end

  @spec default() :: t()
  def default() do
    %Base{}
  end

  @spec new(any()) :: t()
  def new(transactions) do
    %Base{transactions: transactions}
  end

  @spec digest(t()) :: binary()
  def digest(term) do
    Anoma.Serializer.digest(term)
  end
end
