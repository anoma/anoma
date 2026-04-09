defmodule Examples.AdvertiseTest do
  use ExUnit.Case, async: false
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EAdvertise,
    exclude: [:stop_slave]
end
