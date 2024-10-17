defmodule Anoma.ClientTest do
  use ExUnit.Case

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EClient
end
