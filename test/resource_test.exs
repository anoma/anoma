defmodule AnomaTest.Resource do
  use TestHelper.TestMacro, async: true
  doctest Anoma.Resource

  alias Examples.{EResource, EProofRecord, ETransaction}

  test "examples" do
    EResource.a_commit()
    EResource.a2_commit()
    EResource.b_commit()
    EResource.a_nullifier()
    EResource.a2_nullifier()
    EResource.b_nullifier()
    EResource.invalid_nullifier()
    EResource.another_a_kind()
    EResource.d0_kind()
    EProofRecord.a10_space_proof()
    ETransaction.empty_transaction()
    ETransaction.unbalanced_transaction()
    ETransaction.invalid_proofs_transaction()
    ETransaction.balanced_transaction()
    ETransaction.invalid_logic_check()
    ETransaction.balanced_d0_logic()
    ETransaction.increment_counter_transaction()
    ETransaction.full_x_for_y()
  end
end
