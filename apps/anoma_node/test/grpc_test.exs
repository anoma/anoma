defmodule GrpcTest do
  use TestHelper.TestMacro
  doctest CommitmentTree

  alias Anoma.Node.Examples.EGRPC

  excluded = [:__struct__]

  EGRPC.__info__(:functions)
  |> Enum.filter(fn {func, arity} ->
    func not in excluded and arity == 0
  end)
  |> Enum.map(fn {func, _arity} ->
    test "#{func}" do
      apply(EGRPC, unquote(func), [])
    end
  end)
end
