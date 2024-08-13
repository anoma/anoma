defmodule AnomaTest.Node.Logger do
  use TestHelper.TestMacro, async: true

  alias Anoma.Node.Router

  setup_all do
    storage = %Anoma.Node.Storage{
      qualified: AnomaTest.Logger.Qualified,
      order: AnomaTest.Logger.Order
    }

    {:ok, router, _} = Router.start()

    {:ok, storage} =
      Router.start_engine(router, Anoma.Node.Storage, storage)

    {:ok, clock} =
      Router.start_engine(router, Anoma.Node.Clock,
        start: System.monotonic_time(:millisecond)
      )

    {:ok, logger_topic} = Router.new_topic(router)

    {:ok, logger} =
      Router.start_engine(router, Anoma.Node.Logger,
        storage: storage,
        clock: clock,
        topic: logger_topic
      )

    {:ok, ordering} =
      Router.start_engine(router, Anoma.Node.Ordering,
        storage: storage,
        logger: logger
      )

    [logger: logger, ordering: ordering, topic: logger_topic, router: router]
  end

  test "Logging succesfull", %{
    logger: logger,
    ordering: ordering,
    topic: topic,
    router: router
  } do
    :ok =
      Router.call(
        router,
        {:subscribe_topic, topic, :local}
      )

    Anoma.Node.Ordering.reset(ordering)

    id = ordering.id.encrypt

    assert_receive(
      {:"$gen_cast", {_, _, {:logger_add, ^id, _msg}}},
      5000
    )

    {list, _msg} = Anoma.Node.Logger.get(logger) |> hd()

    {log, ord, _time, atom} = List.to_tuple(list)

    assert log == Noun.Nounable.to_noun(logger.id)
    assert ord == Noun.Nounable.to_noun(id)
    assert atom == Noun.Nounable.to_noun(:debug)
  end
end
