defmodule RPC.IntentPool do
  @moduledoc """
  I serve RPC requests (currently, via gRPC).
  """
  use GRPC.Server, service: AnomaInterface.IntentPool.Service

  @spec add_intent(AnomaInterface.Transaction.t(), GRPC.Server.Stream.t()) ::
          AnomaInterface.Empty.t()
  def add_intent(transaction, _stream) do
    GenServer.cast(:intent_rpc_com, {:add_intent, transaction})
    %AnomaInterface.Empty{}
  end
end
