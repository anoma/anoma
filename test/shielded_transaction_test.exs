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
    EShieldedTransaction.prepare_executor_transaction(:valid)
    EShieldedTransaction.start_worker()
    assert {:ok, :valid_transaction_executed} = EShieldedTransaction.execute_transaction()
  end

  test "invalid shielded transaction" do
    EShieldedTransaction.setup_environment()
    EShieldedTransaction.generate_compliance_proof()
    EShieldedTransaction.create_partial_transaction()
    EShieldedTransaction.create_invalid_shielded_transaction()
    EShieldedTransaction.prepare_executor_transaction(:invalid)
    EShieldedTransaction.start_worker()
    assert {:ok, :invalid_transaction_rejected} = EShieldedTransaction.execute_transaction()
  end
end
