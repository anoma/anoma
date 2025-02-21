defmodule Anoma.Client.Examples.EProxy.Mempool do
  @moduledoc """
  I test the GRPC Proxy its intents endpoint.
  """
  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.ETransaction

  require ExUnit.Assertions

  import Anoma.Client.Examples.EProxy

  @doc """
  I add a transaction to the mempool of the node.
  """
  @spec add_transaction(EClient.t()) :: {EClient.t(), any()}
  def add_transaction(client \\ setup()) do
    # create an arbitrary intent and jam it, and then encode it
    {_, transaction} = ETransaction.trivial_transparent_transaction()

    transaction = Noun.Jam.jam(transaction)

    {:ok, :added} = GRPCProxy.add_transaction(transaction)
    client
  end
end
