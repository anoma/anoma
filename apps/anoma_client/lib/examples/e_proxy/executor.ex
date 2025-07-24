defmodule Anoma.Client.Examples.EProxy.Executor do
  @moduledoc """
  I test the GRPC Proxy its executor endpoint.
  """

  alias Anoma.Client.Examples.EClient
  alias Anoma.Client.Node.GRPCProxy

  require ExUnit.Assertions

  import Anoma.Client.Examples.EProxy
  import ExUnit.Assertions
  import ExUnit.CaptureLog

  @doc """
  I add a read-only transaction to the executor.
  """
  @spec run_scry(EClient.t()) :: {EClient.t(), any()}
  def run_scry(client \\ setup()) do
    # create an arbitrary read-only transaction and jam it
    key =
      "valid"
      |> Noun.Jam.jam()

    # call the proxy
    result = GRPCProxy.run_scry(key)

    # this result is arbitrary and depends on the read only transaction submitted.
    # this could be fixed perhaps.
    assert result == {:error, :absent}
    {client, key}
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec run_invalid_scry(EClient.t()) ::
          {EClient.t(), binary()}
  def run_invalid_scry(client \\ setup()) do
    # invalid jammed nock
    key = "invalid"

    assert capture_log(fn ->
             # call the proxy
             result = GRPCProxy.run_scry(key)

             assert result ==
                      {:error, :run_scry_failed, "invalid nock code"}

             # this sleep ensures that the log is captured before the
             # capture_log wrapper terminates.
             Process.sleep(100)
           end) =~
             "Exception raised while handling /Anoma.Proto.ExecutorService/RunScry"

    {client, key}
  end
end
