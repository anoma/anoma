defmodule RPC.Convert do
  @moduledoc """
  I contain functions to convert between serialisation-format datatypes and our
  own internal datatypes.

  Eventually, I should go away, and we should only use data types defined by
  the serialisation format.  It is annoying to keep three things in sync.  But
  for now, I exist as a hedge to insulate core code against changes in the
  serialisation format.
  """

  import Bitwise

  # no letrec, if I am not mistaken? :(
  # using _ suffix--is there a convention analogous to the * suffix in common lisp?
  @spec serialise_nat_(non_neg_integer()) :: list(non_neg_integer())
  defp serialise_nat_(n) do
    if n == 0 do
      []
    else
      [n &&& ((1 <<< 32) - 1) | serialise_nat_(n >>> 32)]
    end
  end

  # not exactly 'serial', but it gets the point across
  @spec serialise_nat(non_neg_integer()) :: AnomaInterface.Nat.t
  def serialise_nat(n) do
    %AnomaInterface.Nat{digits: serialise_nat_(n)}
  end

  @spec deserialise_nat(AnomaInterface.Nat.t) :: non_neg_integer()
  def deserialise_nat(n) do
    List.foldr(n.digits, 0, fn(x, acc) -> x + (acc <<< 32) end)
  end

  def serialise_int(n) do
    %AnomaInterface.Int{sign: n < 0, magnitude: serialise_nat(abs(n))}
  end
  def deserialise_int(n) do
    res = deserialise_nat(n.magnitude)
    if n.sign do -res else res end
  end

  @spec serialise_resource(Anoma.Resource.t) :: AnomaInterface.Resource.t
  def serialise_resource(r) do
    %AnomaInterface.Resource{
      logic: :erlang.term_to_binary(r.logic),
      label: r.label,
      quantity: serialise_nat(r.quantity),
      data: r.data,
      eph: r.eph,
      nonce: r.nonce,
      npk: r.npk,
      rseed: r.rseed,
    }
  end

  @spec deserialise_resource(AnomaInterface.Resource.t) :: Anoma.Resource.t
  def deserialise_resource(r) do
    %Anoma.Resource{
      logic: :erlang.binary_to_term(r.logic),
      label: r.label,
      quantity: deserialise_nat(r.quantity),
      data: r.data,
      eph: r.eph,
      nonce: r.nonce,
      npk: r.npk,
      rseed: r.rseed,
    }
  end

  @spec serialise_transaction(Anoma.Transaction.t) :: AnomaInterface.Transaction.t
  def serialise_transaction(t) do
    %AnomaInterface.Transaction{
      roots: t.roots,
      commitments: t.commitments,
      nullifiers: t.nullifiers,
      proofs: Enum.map(t.proofs, fn x -> %AnomaInterface.ProofRecord{resource: serialise_resource(x.proof.resource)} end),
    }
  end
  @spec deserialise_transaction(AnomaInterface.Transaction.t) :: Anoma.Transaction.t
  def deserialise_transaction(t) do
    %Anoma.Transaction{
      roots: t.roots,
      commitments: t.commitments,
      nullifiers: t.nullifiers,
      proofs: Enum.map(t.proofs, fn x -> Anoma.ProofRecord.prove(deserialise_resource(x.resource)) end),
    }
  end
end
