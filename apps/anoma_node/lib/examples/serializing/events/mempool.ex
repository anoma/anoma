defmodule Anoma.Node.Examples.Serializing.Events.Mempool do
  @moduledoc """
  I define examples on how to serialize events to json.

  I am merely a test suite, rather than examples.
  """

  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Examples.ETransaction

  import ExUnit.Assertions

  @doc """
  I test the json encoding of an empty tx event
  """
  @spec tx_event :: Mempool.Events.TxEvent.t()
  def tx_event do
    tx_event = %Mempool.Events.TxEvent{}
    json = Jason.encode!(tx_event)

    assert Jason.decode(json) ==
             %{id: nil, tx: nil}
             |> Jason.encode!()
             |> Jason.decode()

    tx_event
  end

  @doc """
  I test the json encoding of a tx event with a tx
  """
  @spec tx_event_with_values() :: Mempool.Events.TxEvent.t()
  def tx_event_with_values do
    tx_event = tx_event()

    # create a random transactoin
    transaction = ETransaction.simple_transaction()
    # create a tx event for this transaction
    tx_event =
      %{
        tx_event
        | id: transaction.id,
          tx: %Mempool.Tx{
            tx_result: transaction.result,
            vm_result: transaction.result,
            backend: transaction.backend
          }
      }

    # encode the transaction into a json string
    json = Jason.encode!(tx_event)

    # assert the result is what we exepct
    expected_result =
      transaction.result |> elem(1) |> Noun.Jam.jam() |> Base.encode64()

    assert Jason.decode(json) ==
             %{
               id: transaction.id,
               tx: %{
                 backend: nil,
                 code: nil,
                 tx_result: expected_result,
                 vm_result: expected_result
               }
             }
             |> Jason.encode!()
             |> Jason.decode()

    tx_event
  end

  @doc """
  I serialize a consensus event with an empty list of transaction ids.
  """
  @spec consensus_event :: Mempool.Events.ConsensusEvent.t()
  def consensus_event do
    consensus_event = %Mempool.Events.ConsensusEvent{}
    json = Jason.encode!(consensus_event)

    assert Jason.decode(json) ==
             %{order: []}
             |> Jason.encode!()
             |> Jason.decode()

    consensus_event
  end

  @doc """
  I serialize a consensus event with an empty list of transaction ids.
  """
  @spec consensus_event_with_ids :: Mempool.Events.ConsensusEvent.t()
  def consensus_event_with_ids do
    consensus_event = %{consensus_event() | order: ["foo", "bar"]}
    json = Jason.encode!(consensus_event)

    assert Jason.decode(json) ==
             %{order: ["foo", "bar"]}
             |> Jason.encode!()
             |> Jason.decode()

    consensus_event
  end

  @doc """
  I serialize an empty block event.
  """
  @spec block_event :: Mempool.Events.BlockEvent.t()
  def block_event do
    block_event = %Mempool.Events.BlockEvent{}
    json = Jason.encode!(block_event)

    assert Jason.decode(json) ==
             %{order: [], round: nil}
             |> Jason.encode!()
             |> Jason.decode()

    block_event
  end

  @doc """
  I serialize an empty block event.
  """
  @spec block_event_with_order_and_round :: Mempool.Events.BlockEvent.t()
  def block_event_with_order_and_round do
    block_event = %{block_event() | round: 123, order: ["foo", "bar"]}

    json = Jason.encode!(block_event)

    assert Jason.decode(json) ==
             %{order: ["foo", "bar"], round: 123}
             |> Jason.encode!()
             |> Jason.decode()

    block_event
  end
end
