defmodule IntentPoolTest do
  use ExUnit.Case, async: true

  alias Anoma.Node.Examples.{EIntentPool, ENode}

  test "intentpool examples" do
    EIntentPool.list_intents(ENode.start_node(node_id: "IP_test1"))
    EIntentPool.add_intent(ENode.start_node(node_id: "IP_test2"))
    EIntentPool.remove_intent(ENode.start_node(node_id: "IP_test3"))

    EIntentPool.add_intent_transaction_nullifier(
      ENode.start_node(node_id: "IP_test3")
    )

    EIntentPool.remove_intents_with_nulllified_resources(
      ENode.start_node(node_id: "IP_test3")
    )

    EIntentPool.add_intent_with_known_nullifiers(
      ENode.start_node(node_id: "IP_test3")
    )
  end
end
