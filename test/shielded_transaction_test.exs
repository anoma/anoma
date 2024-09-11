defmodule AnomaTest.ShieldedTransaction do
  use TestHelper.TestMacro, async: true
  doctest Anoma.ShieldedResource

  alias Examples.ENode.EShieldedExecution

  test "valid shielded transaction" do
    EShieldedExecution.execute_valid_transaction()
  end

   test "invalid shielded transaction" do
     EShieldedExecution.execute_invalid_transaction()
   end

  test "double valid shielded transaction" do
    EShieldedExecution.execute_double_transaction()
  end

end
