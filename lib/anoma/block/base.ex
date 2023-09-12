defmodule Anoma.Block.Base do
  @moduledoc """

  I represent the Base part of a block.

  ### Type

     - `:transactions` - A list of transactions.

  """
  use TypedStruct

  typedstruct do
    field(:transactions, list(Anoma.PartialTx.t()), default: [])
  end


end
