defmodule Examples.ETransaction do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Resource.Transaction

  alias Examples.{EResource, EProofRecord}

  def zero_delta(), do: %{}

  def d0_delta(value) do
    %{EResource.d0_kind() => value}
  end

  @spec empty_transaction() :: Transaction.t()
  def empty_transaction() do
    trans = %Transaction{}
    Transaction.verify(trans)
    trans
  end

  ####################################################################
  ##                      Failing Transactions                      ##
  ####################################################################

  @spec unbalanced_transaction() :: Transaction.t()
  def unbalanced_transaction() do
    trans = %Transaction{
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
    trans = %Transaction{
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
    trans = %Transaction{
      nullifiers: [EResource.a10_d0_nullifier()],
      proofs: [EProofRecord.a10_d0_proof()],
      # TODO Abstract this out
      delta: d0_delta(-EResource.a10_d0_resource().quantity)
    }

    refute Transaction.verify(trans)
    trans
  end

  ####################################################################
  ##                     Succeeding Transactions                    ##
  ####################################################################

  @spec balanced_d0_logic() :: Transaction.t()
  def balanced_d0_logic() do
    invalid = invalid_logic_check()

    trans =
      %Transaction{
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
    trans = %Transaction{
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
    trans = %Transaction{
      commitments: [EResource.a1_counter_commit()],
      nullifiers: [EResource.a0_counter_nullifier()],
      proofs: [
        EProofRecord.a0_counter_proof(),
        EProofRecord.a1_counter_proof()
      ],
      delta: %{EResource.counter_kind() => 1}
    }

    assert Transaction.verify(trans)

    trans
  end
end
