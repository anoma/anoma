defmodule Anoma.Transaction do
  @moduledoc """
  I represent an Anoma Transaction

  I can be viewed as a wrapper over `Anoma.Intent` where I contain the
  intents used in a transaction

  """
  alias __MODULE__
  use TypedStruct

  typedstruct do
    field(:intents, list(Anoma.Intent.t()), default: [])
    field(:transaction, Anoma.PartialTx.t(), require: true)
  end

  @doc """

  Creates a new transaction. the `intents_used` are optional, as one
  can create a fully formed transaction without any intents!

  ### Parameters

    - `transaction` - the transaction
    - `intents_used` - the intents used in forming the transaction

  ### Output

     - The Transaction itself
  """
  @spec new(Anoma.PartialTx.t(), list(Anoma.Intent.t())) :: t()
  def new(transaction, intents_used \\ []) do
    %Transaction{intents: intents_used, transaction: transaction}
  end

  @spec intents(t()) :: list(Anoma.Intent.t())
  def intents(t), do: t.intents

  @spec transaction(t()) :: Anoma.PartialTx.t()
  def transaction(t), do: t.transaction
end
