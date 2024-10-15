defmodule ProxyTest do
  use ExUnit.Case

  alias Anoma.Client.Examples.EProxy

  test "proxy tests" do
    excluded = [start_proxy_for: 0, start_proxy_for: 1]

    EProxy.__info__(:functions)
    |> Enum.filter(&(&1 in excluded))
    |> Enum.each(fn {func, _arity} ->
      apply(EProxy, func, [])
    end)
  end
end
