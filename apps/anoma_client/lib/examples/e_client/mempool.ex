defmodule Anoma.Client.Examples.EClient.Mempool do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the mempool endpoint.
  """
  use Anoma.Client.Web.ConnCase
  use TypedStruct

  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.ETransaction
  alias Noun.Nounable

  import Anoma.Client.Examples.EClient
  import ExUnit.Assertions
  import ExUnit.CaptureLog

  def example_transaction do
    {_, transaction} = ETransaction.trivial_transparent_transaction()

    transaction
    |> Nounable.to_noun()
    |> Noun.Jam.jam()
    |> Base.encode64()
  end

  @doc """
  I add a transaction to the mempool.
  """
  @spec add_transaction(EClient.t()) :: {EClient.t(), String.t()}
  def add_transaction(client \\ setup()) do
    # create an arbitrary intent and jam it, and then encode it
    transaction = example_transaction()

    # the json payload the endpoint expects
    payload = %{
      "transaction" => transaction,
      "transaction_type" => "transparent_resource"
    }

    data =
      client.conn
      |> post(~p"/mempool/add", payload)
      |> json_response(200)

    assert data == %{"message" => "transaction added"}

    {client, transaction}
  end

  @doc """
  I add a transaction to the mempool, but provide faulty code.
  """
  @spec add_transaction_faulty_nock(EClient.t()) :: EClient.t()
  def add_transaction_faulty_nock(client \\ setup()) do
    payload = %{
      "transaction" => "",
      "transaction_type" => "transparent_resource"
    }

    assert capture_log(fn ->
             data =
               client.conn
               |> post(~p"/mempool/add", payload)
               |> json_response(503)

             assert data == %{
                      "error" => "failed to add transaction",
                      "reason" => "invalid nock code"
                    }
           end) =~ "invalid nock code"

    client
  end

  @doc """
  I make a request to ad a transaction but dont provide a transaction
  """
  @spec add_transaction_no_transaction(EClient.t()) :: EClient.t()
  def add_transaction_no_transaction(client \\ setup()) do
    payload = %{}

    data =
      client.conn
      |> post(~p"/mempool/add", payload)
      |> json_response(503)

    assert data == %{"error" => "unknown error"}

    client
  end
end
