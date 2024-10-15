defmodule ProxyTest do
  use ExUnit.Case

  alias Anoma.Client.Examples.EProxy

  excluded = [:start_proxy_for, :__struct__]

  EProxy.__info__(:functions)
  |> Enum.filter(fn {func, arity} ->
    func not in excluded and arity == 0
  end)
  |> Enum.map(fn {func, _arity} ->
    test "#{func}" do
      apply(EProxy, unquote(func), [])
    end
  end)
end
