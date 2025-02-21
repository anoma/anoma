defmodule Examples.AdvertiseTest do
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EAdvertise
end
