defmodule Anoma.Node.Examples.Serializing.Events.Ordering do
  @moduledoc """
  I define examples on how to serialize events to json.

  I am merely a test suite, rather than examples.
  """
  alias Anoma.Node.Transaction.Ordering

  import ExUnit.Assertions

  @doc """
  I test the json encoding of an empty tx event
  """
  @spec order_event :: Ordering.Events.OrderEvent.t()
  def order_event do
    order_event = %Ordering.Events.OrderEvent{tx_id: "foobar"}
    json = Jason.encode!(order_event)

    assert Jason.decode(json) ==
             %{tx_id: "foobar"}
             |> Jason.encode!()
             |> Jason.decode()

    order_event
  end
end
