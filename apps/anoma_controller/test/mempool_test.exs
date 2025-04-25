defmodule MempoolTest do
  use ExUnit.Case, async: true
  use TestHelper.TestMacro
  use TestHelper.GenerateExampleTests, for: Anoma.Controller.Examples.Mempool
end
