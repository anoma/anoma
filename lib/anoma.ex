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
    arguments = Burrito.Util.Args.get_arguments()

    # This will invoke start_logic if we want that application
    Anoma.Cli.start_application(arguments)
  end

  def start_logic(use_rocks: rocks_flag) do
    Anoma.Configuration.create_min()

    storage = %Anoma.Node.Storage{
      qualified: Anoma.Qualified,
      order: Anoma.Order,
      rm_commitments: Anoma.RMCommitments
    }

    name = :anoma
    snapshot_path = [:my_special_nock_snaphsot | 0]

    node_settings =
      [
        name: name,
        snapshot_path: snapshot_path,
        storage_data: storage,
        block_storage: :anoma_block,
        ping_time:
          if Application.get_env(name, :env) == :prod do
            10000
          else
            :no_timer
          end
      ]
      |> Anoma.Node.start_min()

    children = [
      {Anoma.Node,
       [
         new_storage: true,
         name: name,
         use_rocks: rocks_flag,
         settings: node_settings
       ]}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Anoma)
  end
end
