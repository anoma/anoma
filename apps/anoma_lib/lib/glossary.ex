defmodule Glossary do
  @transaction_candidate """
  A transaction candidate is `t:Noun.t/0` that evaluates to a valid or
  invalid `transaction` for a specified
  `t:Anoma.Node.Executor.Worker.backend/0`
  """
  @doc @transaction_candidate
  @spec transaction_candidate() :: String.t()
  def transaction_candidate(), do: @transaction_candidate
end
