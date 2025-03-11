defprotocol Anoma.RM.Transaction do
  @moduledoc """
  I am the Transaction protocol.

  Use me when you want to write logic over transactions.

  Transactions practically speaking be `Noun.Nounable`
  """

  @doc """
  I compose two transactions into a new transaction
  """
  @fallback_to_any true

  @spec compose(t(), t()) :: t()
  def compose(tx1, tx2)

  @spec verify(t()) :: true | {:error, String.t()}
  def verify(transaction)

  @spec commitments(t()) :: MapSet.t(binary())
  def commitments(transaction)

  @spec nullifiers(t()) :: MapSet.t(binary())
  def nullifiers(transaction)

  @spec compose_pre_check(t(), t()) :: boolean()
  def compose_pre_check(tx1, tx2)
end
