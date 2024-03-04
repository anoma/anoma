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

    node_settings = [
      name: name,
      snapshot_path: snapshot_path,
      storage: storage,
      block_storage: :anoma_block
    ]

    children = [
      if Application.get_env(name, :env) == :prod do
        {Anoma.Node, [{:ping_time, 10000} | node_settings]}
      else
        {Anoma.Node, [{:ping_time, :no_timer} | node_settings]}
      end
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Anoma)
  end
end
