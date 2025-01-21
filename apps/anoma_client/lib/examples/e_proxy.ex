defmodule Anoma.Client.Examples.EProxy do
  @moduledoc """
  I contain examples for the GRPC proxy.

  The proxy is started, and if necessary, a node is started too.

  I then test each public API of the proxy to ensure it works as expected.
  """
  use TypedStruct

  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Client.Examples.EClient
  alias Anoma.Protobuf.Intent
  alias Anoma.Protobuf.Intents.Intent
  alias Examples.ETransparent.ETransaction
  alias Noun.Nounable

  require ExUnit.Assertions

  import ExUnit.Assertions

  ############################################################
  #                    State                                 #
  ############################################################

  ############################################################
  #                    Helpers                               #
  ############################################################

  @spec setup() :: EClient.t()
  def setup() do
    EClient.create_example_client()
  end

  ############################################################
  #                    Examples                              #
  ############################################################

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_intents(EClient.t()) :: {EClient.t(), any()}
  def list_intents(client \\ setup()) do
    expected_intents = []

    # call the proxy
    {:ok, response} = GRPCProxy.list_intents()

    # assert the result is what was expected
    assert response.intents == expected_intents

    {client, response.intents}
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec add_intent(EClient.t()) :: EClient.t()
  def add_intent(client \\ setup()) do
    # create an arbitrary intent and jam it
    intent_jammed =
      ETransaction.nullify_intent()
      |> Nounable.to_noun()
      |> Nock.Jam.jam()

    # intent to add
    intent = %Intent{intent: intent_jammed}

    # call the proxy
    result = GRPCProxy.add_intent(intent)

    # assert the call succeeded
    assert Kernel.match?({:ok, %{result: "intent added"}}, result)

    client
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_nullifiers(EClient.t()) :: {EClient.t(), [any()]}
  def list_nullifiers(client \\ setup()) do
    expected_nullifiers = ["null", "ifier"]

    # call the proxy
    {:ok, response} = GRPCProxy.list_nullifiers()

    # assert the result is what was expected
    assert expected_nullifiers == response.nullifiers

    {client, response.nullifiers}
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_unrevealed_commits(EClient.t()) :: {EClient.t(), [any()]}
  def list_unrevealed_commits(client \\ setup()) do
    expected_commits = ["commit1", "commit2"]

    # call the proxy and assert the result is what was expected
    {:ok, response} = GRPCProxy.list_unrevealed_commits()

    # assert the result is what was expected
    assert response.commits == expected_commits

    {client, response.commits}
  end

  @doc """
  I ask the node to return its list of intents via the proxy.
  """
  @spec list_unspent_resources(EClient.t()) :: {EClient.t(), [any()]}
  def list_unspent_resources(client \\ setup()) do
    expected_resources = ["unspent resource 1", "unspent resource 2"]

    # call the proxy
    {:ok, result} = GRPCProxy.list_unspent_resources()

    # assert the result is what was expected
    assert expected_resources == result.unspent_resources

    {client, result.unspent_resources}
  end
end
