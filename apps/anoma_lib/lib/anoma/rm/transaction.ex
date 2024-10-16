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

# ----------------------------------------------------------------------------
# Implementation for Anoma.RM.DumbIntent

defimpl Anoma.RM.Transaction, for: Anoma.RM.DumbIntent do
  alias Anoma.RM.DumbIntent

  @impl true
  def compose(%DumbIntent{} = intent_1, %DumbIntent{} = intent_2) do
    %DumbIntent{value: intent_1.value + intent_2.value}
  end

  @impl true
  def verify(%DumbIntent{} = intent) do
    intent.value == 0
  end

  @impl true
  def storage_commitments(_) do
    raise "Not implemented"
  end

  @impl true
  def storage_nullifiers(_) do
    raise "Not implemented"
  end

  @impl true
  def commitments(_) do
    raise "Not implemented"
  end

  @impl true
  def nullifiers(_) do
    raise "Not implemented"
  end

  @impl true
  def cm_tree(_, _storage) do
    raise "Not implemented"
  end

  @impl true
  def resource_existence_check(_, _storage) do
    raise "Not implemented"
  end
end

# ----------------------------------------------------------------------------
# Implementation for Anoma.TransparentResource.Transaction

defimpl Anoma.RM.Transaction, for: Anoma.TransparentResource.Transaction do
  alias Anoma.TransparentResource.Transaction

  # https://specs.anoma.net/latest/system_architecture/state/resource_machine/transaction.html
  @impl true
  def compose(%Transaction{} = tx_1, %Transaction{} = tx_2) do
    Transaction.compose(tx_1, tx_2)
  end

  @impl true
  def verify(%Transaction{} = tx) do
    Transaction.verify(tx)
  end

  @impl true
  def storage_commitments(%Transaction{} = _tx) do
    raise "Not implemented"
  end

  @impl true
  def storage_nullifiers(%Transaction{} = _tx) do
    raise "Not implemented"
  end

  @impl true
  def commitments(%Transaction{} = _tx) do
    raise "Not implemented"
  end

  @impl true
  def nullifiers(%Transaction{} = _tx) do
    raise "Not implemented"
  end

  @impl true
  def cm_tree(%Transaction{} = _tx, _storage) do
    raise "Not implemented"
  end

  @impl true
  def resource_existence_check(%Transaction{} = _tx, _storage) do
    raise "Not implemented"
  end
end
