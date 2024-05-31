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
  end

  test "Juvix counter logic transaction" do
    # Compile
    # https://github.com/anoma/juvix-anoma/blob/c1703f1b30417a5ff980921f948b44abc6becd26/examples/RawCounterTransaction.juvix
    # using:
    #
    #   juvix compile anoma RawCounterTransaction.juvix
    #
    # and copy the output file `RawCounterTransaction.nockma` to the root of this project.
    {:ok, contents} = File.read("RawCounterTransaction.nockma")
    transactionBuilder = Noun.Format.parse_always(contents)

    call = [9, 2, 0 | 1]
    {:ok, counterTx} = Nock.nock(transactionBuilder, call)

    tx = Transaction.from_noun(counterTx)

    assert Transaction.verify(tx), "failed to verify"
  end
end
