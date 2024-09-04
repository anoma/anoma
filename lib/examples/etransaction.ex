defmodule Examples.ETransaction do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.RM.Resource.Delta
  alias Anoma.RM.Resource.Transaction, as: TTransaction
  alias Anoma.RM.Transaction

  alias Examples.{EResource, EProofRecord}

  def zero_delta() do
    delta = Delta.empty()

    assert Delta.negate(delta) == delta,
           "negation of the empty delta is itself"

    delta
  end

  def d0_delta(value) do
    Delta.new(%{EResource.d0_kind() => value})
  end

  @spec empty_transaction() :: Transaction.t()
  def empty_transaction() do
    trans = %TTransaction{}
    Transaction.verify(trans)
    trans
  end

  ####################################################################
  ##                      Failing Transactions                      ##
  ####################################################################

  @spec unbalanced_transaction() :: Transaction.t()
  def unbalanced_transaction() do
    trans = %TTransaction{
      commitments: [EResource.a5_space_commit()],
      nullifiers: [EResource.a10_space_nullifier()],
      proofs: [EProofRecord.a10_space_proof(), EProofRecord.a5_space_proof()],
      delta: zero_delta()
    }

    refute Transaction.verify(trans)
    trans
  end

  @spec invalid_proofs_transaction() :: Transaction.t()
  def invalid_proofs_transaction() do
    trans = %TTransaction{
      commitments: [EResource.b10_space_commit()],
      nullifiers: [EResource.a10_space_nullifier()],
      proofs: [EProofRecord.a10_space_proof(), EProofRecord.a5_space_proof()],
      delta: zero_delta()
    }

    refute Transaction.verify(trans)
    trans
  end

  @spec invalid_logic_check() :: Transaction.t()
  def invalid_logic_check() do
    trans = %TTransaction{
      nullifiers: [EResource.a10_d0_nullifier()],
      proofs: [EProofRecord.a10_d0_proof()],
      # TODO Abstract this out
      delta: d0_delta(-EResource.a10_d0_resource().quantity)
    }

    refute Transaction.verify(trans)
    trans
  end

  ####################################################################
  ##                      Partial Transactions                      ##
  ####################################################################

  def ax_for_y() do
    trans = %TTransaction{
      nullifiers: [EResource.ax_nullifier()],
      commitments: [EResource.ay_commit()],
      # get proof records
      proofs: [EProofRecord.ay_proof(), EProofRecord.ax_proof()],
      # TODO Abstract this out
      delta: Delta.new(%{EResource.y_kind() => 1, EResource.x_kind() => -1})
    }

    refute Transaction.verify(trans)
    trans
  end

  def by_for_x() do
    trans = %TTransaction{
      nullifiers: [EResource.by_nullifier()],
      commitments: [EResource.bx_commit()],
      # get proof records
      proofs: [EProofRecord.by_proof(), EProofRecord.bx_proof()],
      # TODO Abstract this out
      delta: Delta.new(%{EResource.y_kind() => -1, EResource.x_kind() => 1})
    }

    refute Transaction.verify(trans)
    trans
  end

  ####################################################################
  ##                     Succeeding Transactions                    ##
  ####################################################################

  def full_x_for_y() do
    trans = Transaction.compose(by_for_x(), ax_for_y())
    assert Transaction.verify(trans)
    trans
  end

  @spec balanced_d0_logic() :: Transaction.t()
  def balanced_d0_logic() do
    invalid = invalid_logic_check()

    trans =
      %TTransaction{
        invalid
        | commitments: [EResource.b10_d0_commit()],
          delta: zero_delta(),
          proofs: [EProofRecord.b10_d0_proof() | invalid.proofs]
      }

    assert Transaction.verify(trans)
    trans
  end

  @spec balanced_transaction() :: Transaction.t()
  def balanced_transaction() do
    trans = %TTransaction{
      commitments: [EResource.b10_space_commit()],
      nullifiers: [EResource.a10_space_nullifier()],
      proofs: [EProofRecord.a10_space_proof(), EProofRecord.b10_space_proof()],
      delta: zero_delta()
    }

    assert Transaction.verify(trans)
    trans
  end

  @spec increment_counter_transaction() :: Transaction.t()
  def increment_counter_transaction() do
    trans = %TTransaction{
      commitments: [EResource.a1_counter_commit()],
      nullifiers: [EResource.a0_counter_nullifier()],
      proofs: [
        EProofRecord.a0_counter_proof(),
        EProofRecord.a1_counter_proof()
      ],
      delta: Delta.new(%{EResource.counter_kind() => 1})
    }

    assert Transaction.verify(trans)

    trans
  end
end
