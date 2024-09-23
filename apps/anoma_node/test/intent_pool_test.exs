defmodule IntentPoolTest do
  use ExUnit.Case, async: false

  alias Anoma.Node.Examples.EIntentPool

  test "intentpool examples" do
    EIntentPool.list_intents()
    EIntentPool.add_intent()
    EIntentPool.remove_intent()
  end
end
