defmodule RPC.IntentCommunicator do
  @moduledoc """
  I act as an intermediary between the intent pool RPC server and the pool
  GenServer.

  My name is hardcoded as :intent_rpc_com because it appears there is no way to
  have a stateful gRPC server, so there is no way to parametrise it according
  to me.
  """

  use GenServer
  alias Anoma.Node.Intent

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: :intent_rpc_com)
  end

  def init(pool_com) do
    {:ok, pool_com}
  end

  def handle_cast({:add_intent, intent}, pool_com) do
    Intent.Communicator.new_intent(
      pool_com,
      RPC.Convert.deserialise_transaction(intent)
    )

    {:noreply, pool_com}
  end
end
