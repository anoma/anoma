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

  # Optimus.t() is opaque so the help fails to type check, but it's OK
  @dialyzer {:nowarn_function, start: 2}
  def start(_type, _args) do
    arguments = Burrito.Util.Args.get_arguments()

    case Optimus.parse(Anoma.Cli.argument_parser(), arguments) do
      # This will occur when you launch your repl
      {:ok, %{flags: %{nohalt: true}}} ->
        start_logic()

      # This will occur when one tries to test the codebase
      {:ok, %{unknown: [_, "test" | _]}} ->
        start_logic()

      {:ok, [:nockma], parsed} ->
        Nock.Cli.main(parsed)
        System.halt(0)

      :help ->
        IO.puts(Optimus.help(Anoma.Cli.argument_parser()))
        System.halt(0)

      _ ->
        IO.puts(Optimus.help(Anoma.Cli.argument_parser()))
        System.halt(1)
    end
  end

  def start_logic() do
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
