defmodule Anoma.Node.Examples.EExecutor do
  alias Anoma.Node.Examples.ETransaction
  alias Noun

  @doc """
  I create a simple read-only transaction to be used in tests.
  """
  @spec read_only_transaction :: Noun.t()
  def read_only_transaction() do
    {_backend, code} = ETransaction.zero()
    code
  end
end
