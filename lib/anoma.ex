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
    Anoma.Mnesia.init()
    arguments = Burrito.Util.Args.get_arguments()

    # This will invoke start_logic if we want that application
    Anoma.Cli.start_application(arguments)
  end

  @doc """
  I start the Anoma application.

  Given environment `env` I search for a configuration file
  `anoma_env.toml` in the appropriate configuration direction. If the
  configuration refers to a dumped session, we launch it directly.
  Otherwise we launch it with minimal settings.

  If no configuraton was found, I provide basic setup for a new Node and
  start it under supervision.
  """
  def start_logic(use_rocks: rocks_flag) do
    config_path =
      "anoma_#{Mix.env()}.toml" |> Anoma.System.Directories.configuration()

    if config_path |> File.exists?() == true do
      case (config_path |> Anoma.Dump.load())[:dump] do
        nil ->
          Anoma.Configuration.launch_min(config_path, Anoma)

        file ->
          Anoma.Dump.launch(file, :anoma, Anoma)
      end
    else
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
end
