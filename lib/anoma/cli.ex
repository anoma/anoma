defmodule Anoma.Cli do
  @spec argument_parser() :: Optimus.t()
  def argument_parser() do
    Optimus.new!(
      name: "anoma",
      description: """
      Starts up Anoma.
      """,
      allow_unknown_args: true,
      parse_double_dash: true,
      args: [],
      flags: [
        nohalt: [
          long: "--no-halt",
          help: "typical IEX command",
          required: false
        ],
        no_rocksdb: [
          long: "--no-rocks",
          help: "Do not use the rocksdb mnesia backend",
          required: false
        ]
      ],
      options: [],
      subcommands: [
        nockma: Nock.Cli.argument_option()
      ]
    )
  end

  @doc """
  Provides taking CLI argument parsing to arguments used by the
  application
  """
  @spec cli_arguments_to_start_arguments(Optimus.ParseResult.t()) ::
          Keyword.t()
  def cli_arguments_to_start_arguments(%Optimus.ParseResult{
        args: _args,
        flags: %{no_rocksdb: not_rocks},
        options: _options,
        unknown: _unknown
      }) do
    [use_rocks: not not_rocks]
  end

  @spec start_application([String.t()]) :: {:ok, pid()} | {:error, any()}
  def start_application(arguments) do
    start_anoma = fn parsed ->
      parsed
      |> cli_arguments_to_start_arguments()
      |> Anoma.start_logic()
    end

    case Optimus.parse(Anoma.Cli.argument_parser(), arguments) do
      # This will occur when you launch your repl
      {:ok, args = %{flags: %{nohalt: true}}} ->
        start_anoma.(args)

      # This will occur when one tries to test the codebase
      {:ok, args = %{unknown: [_, "test" | _]}} ->
        start_anoma.(args)

      # Happens on various debug tooling
      {:ok, args = %{unknown: [debugger]}} ->
        if debugger |> String.contains?("elixir-ls") do
          start_anoma.(args)
        else
          top_level_help()
          System.halt(1)
        end

      {:ok, [:nockma], parsed} ->
        Nock.Cli.main(parsed)
        System.halt(0)

      :help ->
        # The default parse! will quit
        Optimus.parse!(Anoma.Cli.argument_parser(), arguments)

      # Happens on sub commands as well
      {:help, _} ->
        Optimus.parse!(Anoma.Cli.argument_parser(), arguments)

      _ ->
        top_level_help()
        System.halt(1)
    end
  end

  # Optimus.t() is opaque so the help fails to type check, but it's OK
  @dialyzer {:nowarn_function, top_level_help: 0}
  def top_level_help() do
    IO.puts(Optimus.help(Anoma.Cli.argument_parser()))
  end
end
