defmodule RPC.IntentForwarder do
  @moduledoc """
  I forward intents from the intent pool to the intent solver, due to
  limitations in our gRPC implementation.
  """

  use GenServer

  def start_link(init) do
    GenServer.start_link(__MODULE__, init)
  end

  def init({intent_com, solver_host, solver_port}) do
    {:ok, solver} =
      GRPC.Stub.connect(solver_host <> ":" <> to_string(solver_port))

    Anoma.Node.Intent.Communicator.subscribe(intent_com, self())
    {:ok, solver}
  end

  def handle_cast({:intents, intents}, solver) do
    Enum.each(intents, fn x ->
      AnomaInterface.Solver.Stub.add_intent(
        solver,
        RPC.Convert.serialise_transaction(x)
      )
    end)

    {:noreply, solver}
  end

  def handle_cast({:new_intent, intent}, solver) do
    AnomaInterface.Solver.Stub.add_intent(
      solver,
      RPC.Convert.serialise_transaction(intent)
    )

    {:noreply, solver}
  end

  def handle_cast({:remove_intent, intent}, solver) do
    AnomaInterface.Solver.Stub.del_intent(
      solver,
      RPC.Convert.serialise_transaction(intent)
    )

    {:noreply, solver}
  end
end
