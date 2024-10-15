defmodule Anoma.ClientTest do
  use ExUnit.Case

  alias Anoma.Client.Examples.EClient

  excluded = []

  EClient.__info__(:functions)
  |> Enum.filter(fn {func, arity} ->
    func not in excluded and arity == 0
  end)
  |> Enum.map(fn {func, _arity} ->
    test "#{func}" do
      apply(EClient, unquote(func), [])
    end
  end)
end
