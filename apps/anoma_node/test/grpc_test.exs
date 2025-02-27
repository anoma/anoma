defmodule Anoma.Node.Examples.GrpcTest do
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EGRPC
end
