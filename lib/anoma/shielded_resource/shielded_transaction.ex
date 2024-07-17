defmodule Anoma.ShieldedResource.ShieldedTransaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  require Logger

  alias __MODULE__
  use TypedStruct
  alias Anoma.ShieldedResource.PartialTransaction

  typedstruct enforce: true do
    # TODO: The roots, commitments, and nullifiers can be eliminated. We can
    # obtain them from public inputs. Then we can make the same improvement for
    # transparent transactions. However, they are used in the executor atm.
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:partial_transactions, list(PartialTransaction.t()), default: [])
    field(:delta, binary(), default: %{})
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(transaction = %ShieldedTransaction{}) do
    [
      transaction.roots,
      transaction.commitments,
      transaction.nullifiers,
      for ptx <- transaction.partial_transactions do
        PartialTransaction.to_noun(ptx)
      end,
      transaction.delta
    ]
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([
        roots,
        commitments,
        nullifiers,
        partial_transactions,
        delta
      ]) do
    %ShieldedTransaction{
      roots: roots,
      commitments: commitments,
      nullifiers: nullifiers,
      partial_transactions:
        for ptx <- partial_transactions do
          PartialTransaction.from_noun(ptx)
        end,
      delta: delta
    }
  end

  @spec compose(t(), t()) :: t()
  def compose(tx1, tx2) do
    # I still don't know if proofs have to be unique...
    if Enum.any?(tx1.commitments, fn x -> x in tx2.commitments end) ||
         Enum.any?(tx1.nullifiers, fn x -> x in tx2.nullifiers end) do
      nil
    else
      %ShieldedTransaction{
        roots: tx1.roots ++ tx2.roots,
        commitments: tx1.commitments ++ tx2.commitments,
        nullifiers: tx1.nullifiers ++ tx2.nullifiers,
        partial_transactions:
          tx1.partial_transactions ++ tx2.partial_transactions,
        # fix delta when adding binding signature
        delta: tx1.delta
      }
    end
  end

  # TODO: We can return roots, commitments, and nullifiers instead of just a
  # boolean value so that we can get rid of them in the ShieldedTransaction struct. We
  # can apply the same improvement to the transparent Transaction.
  @spec verify(t()) :: boolean()
  def verify(transaction) do
    # check proofs
    all_proofs_valid =
      for ptx <- transaction.partial_transactions,
          reduce: true do
        acc ->
          result = PartialTransaction.verify(ptx)
          Logger.debug("partial_transactions result: #{inspect(result)}")
          acc && result
      end

    # TODO: other checks

    all_proofs_valid
  end
end
