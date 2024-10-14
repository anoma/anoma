defmodule ClientTest do
  use ExUnit.Case

  alias Client.Examples.EClient

  test "client tests" do
    excluded = []

    EClient.__info__(:functions)
    |> Enum.filter(&(&1 not in excluded))
    |> Enum.each(fn {func, _arity} ->
      apply(EClient, func, [])
    end)
  end
end
