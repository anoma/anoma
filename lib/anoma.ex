defmodule Anoma do
  use Application

  @moduledoc """
  Documentation for `Anoma`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Anoma.hello()
      :world

  """
  def hello do
    :world
  end

  def start(_type, _args) do
    storage = %Anoma.Storage{
      qualified: Anoma.Qualified,
      order: Anoma.Order
    }

    name = :anoma
    snapshot_path = [:my_special_nock_snaphsot | 0]

    children = [
      {Anoma.Node,
       name: name,
       snapshot_path: snapshot_path,
       storage: storage,
       block_storage: :anoma_block},
      {Anoma.Node.Intent, :anoma_intent}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Anoma)
  end
end
