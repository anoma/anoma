defmodule AnomaTest.ShieldedTransaction do
  use TestHelper.TestMacro, async: true
  doctest Anoma.ShieldedResource

  alias Burrito.Util.ERTSUniversalMachineFetcher
  alias Examples.EShieldedTransaction

  test "valid shielded transaction" do
    EShieldedTransaction.setup_environment()
    EShieldedTransaction.generate_compliance_proof()
    EShieldedTransaction.create_partial_transaction()
    EShieldedTransaction.create_shielded_transaction()
    EShieldedTransaction.execute_transaction(:valid)
  end

    test "valid shielded transaction 2" do
      EShieldedTransaction.setup_environment()
      EShieldedTransaction.generate_compliance_proof()
      EShieldedTransaction.create_partial_transaction()
      EShieldedTransaction.create_shielded_transaction_2()
      EShieldedTransaction.execute_transaction(:valid)
    end


  test "invalid shielded transaction" do
    EShieldedTransaction.setup_environment()
    EShieldedTransaction.generate_compliance_proof()
    EShieldedTransaction.create_partial_transaction()
    EShieldedTransaction.create_invalid_shielded_transaction()
    EShieldedTransaction.execute_transaction(:invalid)
  end
end
