defmodule ProxyTest do
  use ExUnit.Case

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EProxy
end
