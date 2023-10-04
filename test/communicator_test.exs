defmodule AnomaTest.Communicator do
  use ExUnit.Case, async: true

  import Anoma.Node.Communicator

  alias Anoma.Node.Communicator

  alias Anoma.Subscriber.Basic

  doctest(Anoma.Node.Communicator)

end
