defmodule Anoma.Client.ProveTEst do
  use ExUnit.Case

  @moduletag :juvix

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EProve
end
