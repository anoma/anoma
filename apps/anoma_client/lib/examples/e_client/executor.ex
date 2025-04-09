defmodule Anoma.Client.Examples.EClient.Executor do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the intents endpoint.
  """
  use Anoma.Client.Web.ConnCase
  use TypedStruct

  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.EExecutor

  import ExUnit.Assertions
  import Anoma.Client.Examples.EClient

  @doc """
  I add a read-only transaction using the client.
  """
  @spec add_read_only_transaction(EClient.t()) :: {EClient.t(), String.t()}
  def add_read_only_transaction(client \\ setup()) do
    # create an arbitrary read-only transaction and jam it
    transaction =
      EExecutor.read_only_transaction()
      |> Noun.Jam.jam()
      |> Base.encode64()

    # the json payload the endpoint expects
    payload = %{"transaction" => transaction}

    data =
      client.conn
      |> post(~p"/executor", payload)
      |> json_response(200)

    # this result is arbitrary and depends on the read only transaction submitted.
    # this could be fixed perhaps.
    assert data == %{"result" => "FfDWyvIq"}

    {client, transaction}
  end
end
