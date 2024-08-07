defmodule AnomaTest.ShieldedTransaction do
  use TestHelper.TestMacro, async: true
  doctest Anoma.ShieldedResource

  alias Burrito.Util.ERTSUniversalMachineFetcher
  alias Examples.EShieldedTransaction

  test "examples" do
    EShieldedTransaction.setup_environment()
    EShieldedTransaction.generate_compliance_proof()
    EShieldedTransaction.create_partial_transaction()
    EShieldedTransaction.create_shielded_transaction()
    EShieldedTransaction.prepare_executor_transaction()
    EShieldedTransaction.start_worker()
    EShieldedTransaction.execute_transaction()
  end
end
