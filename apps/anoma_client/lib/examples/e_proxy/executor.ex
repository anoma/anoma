defmodule Anoma.Client.Examples.EProxy.Executor do
  @moduledoc """
  I test the GRPC Proxy its executor endpoint.
  """

  alias Anoma.Client.Examples.EClient
  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Node.Examples.EExecutor

  require ExUnit.Assertions

  import Anoma.Client.Examples.EProxy
  import ExUnit.Assertions
  import ExUnit.CaptureLog

  @doc """
  I add a read-only transaction to the executor.
  """
  @spec add_read_only_transaction(EClient.t()) :: {EClient.t(), any()}
  def add_read_only_transaction(client \\ setup()) do
    # create an arbitrary read-only transaction and jam it
    transaction =
      EExecutor.read_only_transaction()
      |> Noun.Jam.jam()

    # call the proxy
    result = GRPCProxy.add_read_only_transaction(transaction)

    # this result is arbitrary and depends on the read only transaction submitted.
    # this could be fixed perhaps.
    assert result == {:ok, [[["key" | ""] | ""] | ""]}
    {client, transaction}
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec add_invalid_read_only_transaction(EClient.t()) ::
          {EClient.t(), binary()}
  def add_invalid_read_only_transaction(client \\ setup()) do
    # invalid jammed nock
    transaction = "invalid"

    assert capture_log(fn ->
             # call the proxy
             result = GRPCProxy.add_read_only_transaction(transaction)

             assert result ==
                      {:error, :add_read_only_transaction_failed,
                       "invalid nock code"}

             # this sleep ensures that the log is captured before the
             # capture_log wrapper terminates.
             Process.sleep(100)
           end) =~
             "Exception raised while handling /Anoma.Proto.ExecutorService/Add"

    {client, transaction}
  end
end
