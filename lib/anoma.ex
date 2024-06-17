defmodule Anoma do
  use Application

  alias Anoma.Configuration

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
    config =
      Configuration.default_configuration_location()
      |> Configuration.read_configuration()

    dump_path = Configuration.locate_dump_file(config)

    if dump_path do
      Anoma.Dump.launch(dump_path, :anoma, Anoma)
    else
      Configuration.launch_min(config, rocks_flag, Anoma)
    end
  end
end
