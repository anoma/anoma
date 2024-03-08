defmodule AnomaTest.Node.Clock do
  use ExUnit.Case, async: true

  alias Anoma.Node.Clock
  alias Anoma.Node.Router

  setup_all do
    {:ok, router} = Router.start()
    {:ok, clock} = Router.start_engine(router, Anoma.Node.Clock, start: 0)

    [server: clock]
  end

  test "Epoch get", %{server: clock} do
    assert Clock.get_epoch(clock) == 0
  end
end
