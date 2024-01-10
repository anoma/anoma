defmodule RPC.Solver do
  @moduledoc """
  I serve RPC requests (currently, via gRPC).
  """
  use GRPC.Server, service: AnomaInterface.Solver.Service

  @spec add_intent(AnomaInterface.Transaction.t, GRPC.Server.Stream.t) :: AnomaInterface.Empty.t
  def add_intent(transaction, _stream) do
    GenServer.cast(:solver_rpc_com, {:add_intent, transaction})
    %AnomaInterface.Empty{}
  end

  @spec del_intent(AnomaInterface.Transaction.t, GRPC.Server.Stream.t) :: AnomaInterface.Empty.t
  def del_intent(transaction, _stream) do
    GenServer.cast(:solver_rpc_com, {:del_intent, transaction})
    %AnomaInterface.Empty{}
  end
end
