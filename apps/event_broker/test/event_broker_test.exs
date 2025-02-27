defmodule EventBroker.EventBrokerTest do
  use ExUnit.Case, async: true

  alias EventBroker.Examples.EEventBroker

  test "examples" do
    EEventBroker.check_self_sub()
    EEventBroker.message_works_trivial()
    EEventBroker.un_subscribing_works_atomic()
    EEventBroker.message_gets_blocked()
    EEventBroker.add_filter_on_top()
    EEventBroker.complex_filter_message()
    EEventBroker.non_filters_fail()
    # EEventBroker.kill_subscriber()
  end
end
