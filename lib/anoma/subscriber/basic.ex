defmodule Anoma.Subscriber.Basic do
  @moduledoc """
  I'm a mock subscriber module. I should be able to handle:

  - `{:new_intent, intent}` messages

  And I should:

  - Be able to respond and do computations with these intents.

  - Ping the starting process telling them I properly received an intent
    + This only happens when given a PID to start with
    + This makes me useful for testing

  I am intended to serve as an example subscriber used for testing,
  real subscribers should do what I do and much more

  My functionality are:

  1. Dump the intents given to us

  """

  # TODO Build a query subscriber, where you can query for things

  alias __MODULE__
  use TypedStruct
  use GenServer
  alias Anoma.Intent

  typedstruct do
    field(:intents, list(Intent.t()), default: [])
    field(:home, pid())
  end

  def init(init: init_intents, home: pid) do
    {:ok, %Basic{intents: init_intents, home: pid}}
  end

  def init(init: init_intents) do
    {:ok, %Basic{intents: init_intents}}
  end

  def dump_state(basic) do
    GenServer.call(basic, :dump_state)
  end

  # getting called dynamically typically
  def new_intent(basic, intent) do
    GenServer.call(basic, {:new_intent, intent})
  end

  def handle_cast({:new_intent, intent}, basic) do
    new_basic = %Basic{basic | intents: [intent | basic.intents]}

    if basic.home do
      send(basic.home, :received_intent)
    end

    {:noreply, new_basic}
  end

  def handle_call(:dump_state, _pid, basic) do
    {:reply, basic, basic}
  end
end
