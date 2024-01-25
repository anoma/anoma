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

  def listen_intents(_empty, stream) do
    IO.inspect(stream, label: "new client")

    stream =
      GRPC.Server.send_reply(stream, %AnomaInterface.Transaction{
        roots: [%AnomaInterface.MerkleAnchor{hash: "aaa"}]
      })

    # GRPC.Server.send_reply(stream, %AnomaInterface.Transaction{})
    stream =
      GRPC.Server.send_reply(stream, %AnomaInterface.Transaction{
        roots: [%AnomaInterface.MerkleAnchor{hash: "bbb"}]
      })

    GenServer.cast(:intent_rpc_com, {:new_client, stream})
    2797
  end
end
