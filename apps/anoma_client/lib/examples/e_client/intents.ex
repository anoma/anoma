defmodule Anoma.Client.Examples.EClient.Intents do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the intents endpoint.
  """
  use Anoma.Client.Web.ConnCase

  use TypedStruct

  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.ENode
  alias Examples.ETransparent.ETransaction
  alias Noun.Nounable

  import ExUnit.Assertions
  import Anoma.Client.Examples.EClient

  @doc """
  I list the intents over grpc on the client.

  This test requires an empty node, so it creates a new node and client.
  """
  @spec list_intents_empty :: EClient.t()
  def list_intents_empty do
    node = ENode.start_noded()
    client = create_example_client(node)

    # make a json call, expect an empty list of intents
    data =
      client.conn
      |> get(~p"/intents")
      |> json_response(200)

    assert data == %{"intents" => []}
    client
  end

  @doc """
  I add an intent to the client.
  """
  @spec add_intent(EClient.t()) :: {EClient.t(), String.t()}
  def add_intent(client \\ setup()) do
    # create an arbitrary intent and jam it, and then encode it
    intent =
      ETransaction.nullify_intent()
      |> Nounable.to_noun()
      |> Noun.Jam.jam()
      |> Base.encode64()

    # the json payload the endpoint expects
    payload = %{"intent" => intent}

    data =
      client.conn
      |> post(~p"/intents", payload)
      |> json_response(200)

    assert data == %{"message" => "intent added"}

    {client, intent}
  end

  @doc """
  I add an intent to the client.
  """
  @spec list_intents_not_empty(EClient.t()) :: EClient.t()
  def list_intents_not_empty(client \\ setup()) do
    {client, intent} = add_intent(client)

    # make a json call, expect an empty list of intents
    data =
      client.conn
      |> get(~p"/intents")
      |> json_response(200)

    assert data == %{"intents" => [intent]}
    client
  end
end
