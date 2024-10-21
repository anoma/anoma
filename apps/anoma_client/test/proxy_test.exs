defmodule ProxyTest do
  use ExUnit.Case

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EProxy,
    skip: [:start_proxy_for]
end
