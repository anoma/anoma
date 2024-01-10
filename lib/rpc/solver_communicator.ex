defmodule RPC.SolverCommunicator do
  @moduledoc """
  I act as an intermediary between the solver RPC server and the solver
  GenServer, and forward solved intents to the validator.

  My name is hardcoded as :solver_rpc_com because it appears there is no way to
  have a stateful gRPC server, so there is no way to parametrise it according
  to me.
  """

  use GenServer
  alias Anoma.Node.Solver

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: :solver_rpc_com)
  end

  def init({solver_com, validator_host, validator_port}) do
    # todo have a sensible policy for if this fails to connect, or loses
    # connection (in the latter case, likely exponential backoff)
    {:ok, validator_conn} =
      GRPC.Stub.connect(validator_host <> ":" <> to_string(validator_port))

    Solver.Communicator.subscribe(solver_com, self(), false)
    {:ok, {solver_com, validator_conn}}
  end

  def handle_cast({:add_intent, intent}, {solver_com, validator_conn}) do
    Solver.Communicator.add_intent(
      solver_com,
      RPC.Convert.deserialise_transaction(intent)
    )

    {:noreply, {solver_com, validator_conn}}
  end

  def handle_cast({:del_intent, intent}, {solver_com, validator_conn}) do
    Solver.Communicator.del_intents(solver_com, [
      RPC.Convert.deserialise_transaction(intent)
    ])

    {:noreply, {solver_com, validator_conn}}
  end

  def handle_cast({:solutions, solutions}, {solver_com, validator_conn}) do
    Enum.each(solutions, fn x ->
      AnomaInterface.Validator.Stub.propose_transaction(
        validator_conn,
        RPC.Convert.serialise_transaction(x)
      )
    end)

    {:noreply, {solver_com, validator_conn}}
  end
end
