defmodule Anoma.Node.Examples.Serializing.Structs.Mempool do
  @moduledoc """
  I define examples on how to serialize common structs to json.
  I am merely a test suite, rather than examples.
  """

  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Mempool

  import ExUnit.Assertions

  @doc """
  Serialize an empty Tx struct.
  """
  @spec tx :: Mempool.Tx.t()
  def tx do
    tx = %Mempool.Tx{}
    json = Jason.encode(tx)

    assert json ==
             Jason.encode(%{
               code: nil,
               vm_result: :in_progress,
               tx_result: :in_progress,
               backend: nil
             })

    tx
  end

  @doc """
  I create an event, and put in a noun as the vm result.
  I then assert that this is properly serialized into json as jammed noun.
  """
  @spec tx_with_vm_result :: Mempool.Tx.t()
  def tx_with_vm_result do
    vm_result = {:ok, [[["key"] | 0] | 0]}
    tx = tx() |> Map.put(:vm_result, vm_result)
    json = Jason.encode(tx)

    assert json ==
             Jason.encode(%{
               code: nil,
               vm_result: "FfDWyvIq",
               tx_result: :in_progress,
               backend: nil
             })

    tx
  end

  @doc """
  I create an event, and put in a noun as the vm result, tx result, and code.
  These nouns are arbitrary chosen, they just have to be a noun.

  I then assert that this is properly serialized into json as jammed noun.
  """
  @spec tx_with_nouns :: Mempool.Tx.t()
  def tx_with_nouns do
    # this is just a random noun.
    noun = Examples.ENock.counter_arm()
    encoded_noun = Noun.Jam.jam(noun) |> Base.encode64()

    # put the noun in the tx struct where a noun is possible.
    tx =
      tx()
      |> Map.put(:tx_result, {:ok, noun})
      |> Map.put(:vm_result, {:ok, noun})
      |> Map.put(:code, noun)

    # encode the struct
    json = Jason.encode(tx)

    # assert its serialized correctly
    assert json ==
             Jason.encode(%{
               code: encoded_noun,
               vm_result: encoded_noun,
               tx_result: encoded_noun,
               backend: nil
             })

    tx
  end
end
