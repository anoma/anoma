defmodule AnomaTest.Node.Logger do
  use ExUnit.Case, async: true

  alias Anoma.Node.Router

  setup_all do
    storage = %Anoma.Node.Storage{
      qualified: AnomaTest.Logger.Qualified,
      order: AnomaTest.Logger.Order
    }

    {:ok, router} = Router.start()

    {:ok, storage} =
      Router.start_engine(router, Anoma.Node.Storage, storage)

    {:ok, clock} =
      Router.start_engine(router, Anoma.Node.Clock,
        start: System.monotonic_time(:millisecond)
      )

    {:ok, logger} =
      Router.start_engine(router, Anoma.Node.Logger,
        storage: storage,
        clock: clock
      )

    {:ok, ordering} =
      Router.start_engine(router, Anoma.Node.Storage.Ordering,
        table: storage,
        logger: logger
      )

    [logger: logger, ordering: ordering]
  end

  test "Logging succesfull", %{logger: logger, ordering: ordering} do
    Anoma.Node.Storage.Ordering.state(ordering)

    {list, _msg} = Anoma.Node.Logger.get(logger) |> hd()

    {log, ord, _time, atom} = List.to_tuple(list)

    assert log == logger.id
    assert ord == ordering.id
    assert atom == :info
  end
end
