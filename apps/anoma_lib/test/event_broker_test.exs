defmodule AnomaTest.EventBroker do
  use TestHelper.TestMacro, async: true

  alias Examples.EventBroker

  test "examples" do
    EventBroker.check_self_sub()
    EventBroker.message_works_trivial()
    EventBroker.un_subscribing_works_atomic()
    EventBroker.message_gets_blocked()
    EventBroker.add_filter_on_top()
    EventBroker.complex_filter_message()
    EventBroker.non_filters_fail()
  end
end
