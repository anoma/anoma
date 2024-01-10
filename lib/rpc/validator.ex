defmodule RPC.Validator do
  @moduledoc """
  I serve RPC requests (currently, via gRPC).
  """
  use GRPC.Server, service: AnomaInterface.Validator.Service

  @doc """
  I propose a candidate transaction for inclusion in a block.  Putatively
  """
  @spec propose_transaction(AnomaInterface.Transaction.t, GRPC.Server.Stream.t) :: AnomaInterface.Empty.t
  def propose_transaction(transaction, _stream) do
    tx = RPC.Convert.deserialise_transaction(transaction)
    IO.inspect(Anoma.Transaction.verify(tx), label: "proposed transaction status")
    %AnomaInterface.Empty{}
  end
end
