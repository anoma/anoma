defmodule Anoma.LocalDomain.Examples.EProxy.Intents do
  @moduledoc """
  I test the GRPC Proxy its intents endpoint.
  """

  alias Anoma.LocalDomain.Node.GRPCProxy
  alias Anoma.LocalDomain.Examples.ELocalDomain
  alias Examples.ETransparent.ETransaction
  alias Noun.Nounable

  require ExUnit.Assertions

  import Anoma.LocalDomain.Examples.EProxy
  import ExUnit.Assertions

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_intents(ELocalDomain.t()) :: {ELocalDomain.t(), any()}
  def list_intents(client \\ setup()) do
    {:ok, response} = GRPCProxy.list_intents()
    assert response == []
    client
  end

  @doc """
  I add an intent to the intent pool.
  """
  @spec add_intent(ELocalDomain.t()) :: {ELocalDomain.t(), binary()}
  def add_intent(client \\ setup()) do
    # create an arbitrary intent and jam it
    intent =
      ETransaction.nullify_intent()
      |> Nounable.to_noun()
      |> Noun.Jam.jam()

    # call the proxy
    {:ok, :added} = GRPCProxy.add_intent(intent)

    {client, intent}
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_intents_non_empty(ELocalDomain.t()) ::
          {ELocalDomain.t(), binary()}
  def list_intents_non_empty(client \\ setup()) do
    {client, intent} = add_intent(client)
    {:ok, response} = GRPCProxy.list_intents()
    assert response == [intent]
    {client, intent}
  end
end
