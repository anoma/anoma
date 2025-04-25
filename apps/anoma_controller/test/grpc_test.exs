defmodule Examples.GrpcTest do
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Anoma.Controller.Examples.EGRPC
end
