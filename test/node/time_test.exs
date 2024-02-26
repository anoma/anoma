defmodule AnomaTest.Node.Time do
  use ExUnit.Case, async: true

  alias Anoma.Node.Time.Communicator, as: Tcom

  setup_all do
    unless Process.whereis(:time_clock_com) do
      Anoma.Node.Time.start_link(name: :clock_clock, start: 0)
    end

    [server: :clock_clock_com]
  end

  test "Epoch get", %{server: server} do
    assert Tcom.get_epoch(server) == 0
  end
end
