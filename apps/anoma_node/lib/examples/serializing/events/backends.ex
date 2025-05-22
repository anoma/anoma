defmodule Anoma.Node.Examples.Serializing.Events.Backends do
  @moduledoc """
  I define examples on how to serialize events to json.

  I am merely a test suite, rather than examples.
  """

  alias Anoma.Node.Transaction.Backends.Events

  import ExUnit.Assertions

  @doc """
  I serialize a result_event.
  """
  @spec result_event_error :: Events.ResultEvent.t()
  def result_event_error do
    result_event = %Events.ResultEvent{tx_id: "foo", vm_result: :vm_error}
    _json = Jason.encode!(result_event)
    result_event
  end

  @doc """
  I serialize a result_event.
  """
  @spec result_event :: Events.ResultEvent.t()
  def result_event do
    result_event = %Events.ResultEvent{
      tx_id: "foo",
      vm_result: {:ok, [1 | 2]}
    }

    # this will fail if the encoding doesnt work.
    json = Jason.encode!(result_event)

    # assert the result is what we expect
    expected_result =
      {:ok, [1 | 2]} |> elem(1) |> Noun.Jam.jam() |> Base.encode64()

    assert Jason.decode(json) ==
             %{tx_id: "foo", vm_result: expected_result}
             |> Jason.encode!()
             |> Jason.decode()

    result_event
  end

  @doc """
  I create a minimal complete event and check its serialization.
  """
  @spec complete_event_error :: Events.CompleteEvent.t()
  def complete_event_error do
    complete_event = %Events.CompleteEvent{tx_id: "foo", tx_result: :error}
    json = Jason.encode!(complete_event)

    assert Jason.decode(json) ==
             %{tx_id: "foo", tx_result: :error}
             |> Jason.encode!()
             |> Jason.decode()

    complete_event
  end

  @doc """
  I create a minimal complete event and check its serialization.
  """
  @spec complete_event :: Events.CompleteEvent.t()
  def complete_event do
    complete_event = %Events.CompleteEvent{
      tx_id: "foo",
      tx_result: {:ok, [1 | 2]}
    }

    json = Jason.encode!(complete_event)

    # assert the result is what we expect
    expected_result =
      {:ok, [1 | 2]} |> elem(1) |> Noun.Jam.jam() |> Base.encode64()

    assert Jason.decode(json) ==
             %{tx_id: "foo", tx_result: expected_result}
             |> Jason.encode!()
             |> Jason.decode()

    complete_event
  end

  @doc """
  I create a minimal srm event.
  """
  @spec srme_event :: Events.SRMEvent.t()
  def srme_event do
    srme_event = %Events.SRMEvent{}
    json = Jason.encode!(srme_event)

    assert Jason.decode(json) ==
             %{nullifiers: [], commitments: []}
             |> Jason.encode!()
             |> Jason.decode()

    srme_event
  end

  @doc """
  I create an srm event with some nullifiers and commitments in it.
  """
  @spec srme_event_non_empty :: Events.SRMEvent.t()
  def srme_event_non_empty do
    srme_event = %Events.SRMEvent{
      commitments: MapSet.new(["foo"]),
      nullifiers: MapSet.new(["bar"])
    }

    json = Jason.encode!(srme_event)

    assert Jason.decode(json) ==
             %{commitments: ["foo"], nullifiers: ["bar"]}
             |> Jason.encode!()
             |> Jason.decode()

    srme_event
  end

  @doc """
  I create a minimal srm event.
  """
  @spec trme_event :: Events.TRMEvent.t()
  def trme_event do
    trme_event = %Events.TRMEvent{}
    json = Jason.encode!(trme_event)

    assert Jason.decode(json) ==
             %{commitments: [], nullifiers: []}
             |> Jason.encode!()
             |> Jason.decode()

    trme_event
  end

  @doc """
  I create an srm event with some nullifiers and commitments in it.
  """
  @spec trme_event_non_empty :: Events.TRMEvent.t()
  def trme_event_non_empty do
    trme_event = %Events.TRMEvent{
      commitments: MapSet.new(["foo"]),
      nullifiers: MapSet.new(["bar"])
    }

    json = Jason.encode!(trme_event)

    assert Jason.decode(json) ==
             %{nullifiers: ["bar"], commitments: ["foo"]}
             |> Jason.encode!()
             |> Jason.decode()

    trme_event
  end
end
