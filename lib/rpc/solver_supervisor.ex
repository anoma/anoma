defmodule RPC.SolverSupervisor do
  use Supervisor

  def start_link(state) do
    Supervisor.start_link(__MODULE__, state)
  end

  def init({solver_com, _host, port, validator_host, validator_port}) do
    children = [
      {RPC.SolverCommunicator, {solver_com, validator_host, validator_port}},
      {GRPC.Server.Supervisor,
       endpoint: RPC.SolverEndpoint, port: port, start_server: true}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
