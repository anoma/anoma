defmodule KudosTest do
  use ExUnit.Case

  @moduletag :juvix

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.Apps.Kudos

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.Apps.Kudos.Initialize

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.Apps.Kudos.Transfer

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.Apps.Kudos.Merge

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.Apps.Kudos.Intent
end
