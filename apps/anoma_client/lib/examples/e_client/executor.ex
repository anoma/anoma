defmodule Anoma.Client.Examples.EClient.Executor do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the intents endpoint.
  """
  use Anoma.Client.Web.ConnCase
  use TypedStruct

  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Transaction.Storage, as: NodeStorage

  import ExUnit.Assertions
  import Anoma.Client.Examples.EClient

  @doc """
  I run a scry using the client.
  """
  @spec run_scry(EClient.t()) :: {EClient.t(), String.t()}
  def run_scry(client \\ setup()) do
    val = MapSet.new(["i am a set"])
    key = ["anoma", "blob", "key"]

    NodeStorage.write(
      client.node.node_id,
      {1, [{key, val}]}
    )

    # create an arbitrary read-only transaction and jam it
    transaction =
      ["anoma", "blob", "key"]
      |> Noun.Jam.jam()
      |> Base.encode64()

    # the json payload the endpoint expects
    payload = %{"key" => transaction}

    data =
      client.conn
      |> post(~p"/executor", payload)
      |> json_response(200)

    # this result is arbitrary and depends on the read only transaction submitted.
    # this could be fixed perhaps.
    assert data == %{"result" => "AXzSQMLaQMJA5sroKQ=="}

    {client, transaction}
  end
end
