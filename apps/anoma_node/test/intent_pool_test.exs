defmodule IntentPoolTest do
  use ExUnit.Case, async: true

  alias Anoma.Node.Examples.{EIntentPool, ENode}

  test "intentpool examples" do
    EIntentPool.list_intents(ENode.start_node(node_id: "IP_test1"))
    EIntentPool.add_intent(ENode.start_node(node_id: "IP_test2"))
    EIntentPool.remove_intent(ENode.start_node(node_id: "IP_test3"))
  end
end
