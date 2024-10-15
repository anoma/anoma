defprotocol Anoma.RM.Transaction do
  @moduledoc """
  I am the Transaction protocol.

  Use me when you want to write logic over transactions.

  Transactions practically speaking be `Noun.Nounable`
  """

  @doc """
  I compose two transactions into a new transaction
  """

  @spec compose(t(), t()) :: t()
  def compose(tx1, tx2)

  @spec verify(t()) :: boolean()
  def verify(transaction)

  @spec storage_commitments(t()) :: list(binary())
  def storage_commitments(transaction)

  @spec storage_nullifiers(t()) :: list(binary())
  def storage_nullifiers(transaction)

  @spec commitments(t()) :: list(binary())
  def commitments(transaction)

  @spec nullifiers(t()) :: list(binary())
  def nullifiers(transaction)

  @spec cm_tree(t(), term()) :: CommitmentTree.t()
  def cm_tree(transaction, storage)

  @spec resource_existence_check(t(), pid()) :: boolean()
  def resource_existence_check(transaction, storage)
end

defmodule Anoma.RM.Trans do
  alias Anoma.RM.Transaction
  @spec compose_pre_check(Transaction.t(), Transaction.t()) :: boolean()
  # I still don't know if proofs have to be unique...
  def compose_pre_check(tx1, tx2) do
    {cm1, cm2} = {Transaction.commitments(tx1), Transaction.commitments(tx2)}
    {nf1, nf2} = {Transaction.nullifiers(tx1), Transaction.nullifiers(tx2)}

    not (Enum.any?(cm1, fn x -> x in cm2 end) ||
           Enum.any?(nf1, fn x -> x in nf2 end))
  end
end
