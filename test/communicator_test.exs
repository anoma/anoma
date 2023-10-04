defmodule AnomaTest.Communicator do
  use ExUnit.Case, async: true

  import Anoma.Node.Executor.Communicator

  alias Anoma.Node.Executor.Communicator

  alias Anoma.Subscriber.Basic

  doctest(Anoma.Node.Executor.Communicator)

end
