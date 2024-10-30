defmodule ProveTEst do
  use ExUnit.Case

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EProve
end
