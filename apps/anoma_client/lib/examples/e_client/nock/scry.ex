defmodule Anoma.Client.Examples.EClient.Nock.Scry do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the nock run and prove endpoints.
  """
  use Anoma.Client.Web.ConnCase
  use TypedStruct

  alias Anoma.Client.Examples.EClient
  alias Anoma.Client.Storage
  alias Anoma.Node.Tables
  alias Anoma.Node.Transaction.Storage, as: NodeStorage
  alias Anoma.RM.Transparent.Action
  alias Anoma.RM.Transparent.Transaction
  alias Noun.Jam
  alias Noun.Nounable

  import Anoma.Client.Examples.EClient
  import ExUnit.Assertions

  @spec prove_with_internal_scry_call(EClient.t()) :: EClient.t()
  def prove_with_internal_scry_call(client \\ setup()) do
    :ok = Tables.reset_tables_for_client()

    string = "i am scried"

    action = %Action{app_data: %{<<123>> => [{string, true}]}}

    tx = %Transaction{actions: MapSet.new([action])} |> Nounable.to_noun()

    key = ["anoma", "blob", "key"]
    Storage.write({key, tx})
    program = [[12, [1], 1 | ["id" | key]]] |> Jam.jam() |> Base.encode64()

    # the json payload the endpoint expects
    payload = %{
      "public_inputs" => [],
      "private_inputs" => [],
      "program" => program
    }

    %{"io" => _, "result" => result} =
      client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    # jam the transaction for comparison, because cue'ing the result leads to a different result:
    # [0, [[0, 0, 0, 0, ["{", ["i am scried" | 0] | 0], 0 | 0], 0 | 0] | ""]
    # vs
    # ["", [["", "", "", "", ["{", ["i am scried" | ""] | ""], "" | ""], "" | ""] | ""]
    assert Jam.jam(tx) == Base.decode64!(result)

    # assert the storage value
    assert {:ok, string} ==
             Storage.read({System.os_time(), :crypto.hash(:sha256, string)})

    client
  end

  @spec prove_with_external_scry_call(EClient.t()) :: EClient.t()
  def prove_with_external_scry_call(client \\ setup()) do
    :ok = Tables.reset_tables_for_client()

    key = ["anoma", "blob", "key"]

    NodeStorage.write(
      client.node.node_id,
      {1, [{key, 123}]}
    )

    program =
      [[12, [1], 1 | ["id" | key]]] |> Noun.Jam.jam() |> Base.encode64()

    # the json payload the endpoint expects
    payload = %{"program" => program}

    %{"io" => _, "result" => result} =
      client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    assert Jam.jam(123) == Base.decode64!(result)

    assert Storage.read({System.os_time(), key})
           |> elem(1)
           |> Noun.equal?(123)

    client
  end

  @spec prove_with_external_scry_call_nounify(EClient.t()) :: EClient.t()
  def prove_with_external_scry_call_nounify(client \\ setup()) do
    :ok = Tables.reset_tables_for_client()

    val = MapSet.new(["i am a set"])
    key = ["anoma", "blob", "key"]

    NodeStorage.write(
      client.node.node_id,
      {1, [{key, val}]}
    )

    program =
      [[12, [1], 1 | ["id" | key]]] |> Noun.Jam.jam() |> Base.encode64()

    # the json payload the endpoint expects
    payload = %{"program" => program}

    %{"io" => _, "result" => result} =
      client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    # verify the result
    noun_value = Nounable.to_noun(val)
    noun_result = Base.decode64!(result) |> Jam.cue!() |> Nounable.to_noun()

    assert Noun.equal?(noun_value, noun_result)

    # verify storage
    {:ok, noun_storage} = Storage.read({System.os_time(), key})

    assert Noun.equal?(noun_storage, noun_value)

    client
  end
end
