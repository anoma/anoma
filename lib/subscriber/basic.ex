defmodule Anoma.Subscriber.Basic do
  @moduledoc """
  I'm a mock subscriber module. I should be able to handle:

  - {:new_intent, intent} messages

  And I should:

  - Be able to respond and do computations with these intents.


  I am intended to serve as an example subscriber used for testing,
  real subscribers should do what I do and much more

  My functionality are:

  1. Dump the intents given to us

  """

  # TODO Build a query subscriber, where you can query for things

  alias __MODULE__
  use TypedStruct
  use GenServer

  typedstruct do
    field(:intents, list(), default: [])
  end

  def init(init_intents \\ []) do
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
    {:noreply, %Basic{basic | intents: [intent | basic.intents]}}
  end

  def handle_call(:dump_state, _pid, basic) do
    {:reply, basic, basic}
  end
end
