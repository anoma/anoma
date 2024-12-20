defmodule ProveTEst do
  use ExUnit.Case

  @moduletag :juvix

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EProve
end
