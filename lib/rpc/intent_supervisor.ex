defmodule RPC.IntentSupervisor do
  use Supervisor

  def start_link(state) do
    Supervisor.start_link(__MODULE__, state)
  end

  # seems grpc does not let us tell it what host to listen on--might should
  # send a patch, except that we are probably going to move away from gRPC;
  # either way, leave this here because it's the right interface
  def init({pool_com, _host, port}) do
    children = [
      {RPC.IntentCommunicator, pool_com},
      {GRPC.Server.Supervisor, endpoint: RPC.IntentEndpoint, port: port, start_server: true},
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end

